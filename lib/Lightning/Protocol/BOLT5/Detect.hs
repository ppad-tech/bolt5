{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT5.Detect
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Close identification, output classification, and preimage
-- extraction for BOLT #5 on-chain transaction handling.

module Lightning.Protocol.BOLT5.Detect (
    -- * Close identification
    identify_close

    -- * Output classification
  , classify_local_commit_outputs
  , classify_remote_commit_outputs
  , classify_revoked_commit_outputs

    -- * Preimage extraction
  , extract_preimage_offered
  , extract_preimage_htlc_success

    -- * Timeout check
  , htlc_timed_out
  ) where

import qualified Bitcoin.Prim.Tx as BT
import qualified Crypto.Hash.SHA256 as SHA256
import Data.List (mapAccumL)
import Data.Word (Word32)
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT5.Types

-- close identification -----------------------------------------------

-- | Identify the type of channel close from a transaction that
--   spends the funding output.
--
-- Parses the on-chain transaction and compares its txid against
-- the txids of the known local and remote commitment
-- transactions. Comparing txids rather than bytes means the
-- signed (segwit) serialization found on chain matches the
-- unsigned commitment.
--
-- Returns 'Nothing' if the bytes don't parse as a transaction or
-- the transaction doesn't match either commitment. Mutual close
-- and revoked commitment detection require additional checks by
-- the caller (e.g. comparing closing tx format, checking a secret
-- store for older commitment numbers).
identify_close
  :: CommitmentTx
  -- ^ Our local commitment tx.
  -> CommitmentTx
  -- ^ The remote commitment tx (current).
  -> BS.ByteString
  -- ^ Raw serialized transaction found on chain.
  -> Maybe CloseType
identify_close !localCommitTx !remoteCommitTx
    !onChainBytes =
  case BT.from_bytes onChainBytes of
    Nothing -> Nothing
    Just !onChainTx ->
      let !onChainTxid = Just (BT.txid onChainTx)
      in  if commitment_txid localCommitTx == onChainTxid
          then Just LocalCommitClose
          else if commitment_txid remoteCommitTx == onChainTxid
          then Just RemoteCommitClose
          else Nothing

-- output classification ----------------------------------------------

-- | Classify outputs of our local commitment transaction.
--
-- Per BOLT #5: when we discover our local commitment on chain,
-- we must resolve each output. to_local requires a CSV-delayed
-- spend, to_remote is resolved by the commitment itself, HTLC
-- outputs need second-stage transactions, and anchors can be
-- spent immediately.
--
-- An HTLC output is matched to the HTLC with the same direction,
-- script, amount and CLTV expiry, and each HTLC matches at most one
-- output.
classify_local_commit_outputs
  :: CommitmentTx
  -- ^ Our local commitment transaction.
  -> CommitmentKeys
  -- ^ Derived keys for this commitment.
  -> ToSelfDelay
  -- ^ Remote's to_self_delay (CSV delay for our outputs).
  -> ChannelFeatures
  -- ^ Channel feature flags.
  -> [HTLC]
  -- ^ HTLCs in this commitment.
  -> [UnresolvedOutput]
classify_local_commit_outputs !commitTx !keys !delay
    !features !htlcs =
  case commitment_txid commitTx of
    Nothing -> []
    Just !txid ->
      let !outputs = ctx_outputs commitTx
          !revpk = ck_revocation_pubkey keys
          !delayedpk = ck_local_delayed keys
          classify cands (idx, out) =
            classifyLocalOutput txid revpk delayedpk
              delay features keys cands idx out
      in  snd (mapAccumL classify
                 (htlc_candidates keys features htlcs)
                 (zip [0..] outputs))

-- | Classify a single output from a local commitment tx, removing
--   any matched HTLC from the candidates.
classifyLocalOutput
  :: TxId
  -> RevocationPubkey
  -> LocalDelayedPubkey
  -> ToSelfDelay
  -> ChannelFeatures
  -> CommitmentKeys
  -> [(HTLC, Script)]
  -> Word32
  -> TxOutput
  -> ([(HTLC, Script)], UnresolvedOutput)
classifyLocalOutput !txid !revpk !delayedpk !delay
    !features !keys !cands !idx !out =
  let !op = OutPoint txid idx
      !val = txout_value out
      !(rest, resolution) = case txout_type out of
        OutputToLocal ->
          (cands, SpendToLocal delay revpk delayedpk)
        OutputToRemote ->
          (cands, Resolved)
        OutputLocalAnchor ->
          (cands, AnchorSpend (ck_local_funding keys))
        OutputRemoteAnchor ->
          (cands, Resolved)
        OutputOfferedHTLC expiry ->
          case take_htlc HTLCOffered expiry out cands of
            (Just htlc, cands') ->
              (cands', SpendHTLCTimeout htlc keys features)
            (Nothing, cands') -> (cands', Resolved)
        OutputReceivedHTLC expiry ->
          case take_htlc HTLCReceived expiry out cands of
            (Just htlc, cands') ->
              (cands', SpendHTLCSuccess htlc keys features)
            (Nothing, cands') -> (cands', Resolved)
  in (rest, UnresolvedOutput op val resolution)

-- | Classify outputs of the remote commitment transaction.
--
-- Per BOLT #5: when we discover the remote commitment on chain,
-- there are no CSV delays on our outputs. We can spend offered
-- HTLCs directly after timeout, and received HTLCs directly
-- with the preimage.
--
-- HTLC outputs are matched as in 'classify_local_commit_outputs'.
classify_remote_commit_outputs
  :: CommitmentTx
  -- ^ The remote commitment transaction.
  -> CommitmentKeys
  -- ^ Derived keys for this commitment (from remote's
  --   perspective, so local/remote are swapped).
  -> ChannelFeatures
  -- ^ Channel feature flags.
  -> [HTLC]
  -- ^ HTLCs in this commitment.
  -> [UnresolvedOutput]
classify_remote_commit_outputs !commitTx !keys
    !features !htlcs =
  case commitment_txid commitTx of
    Nothing -> []
    Just !txid ->
      let !outputs = ctx_outputs commitTx
          classify cands (idx, out) =
            classifyRemoteOutput txid keys cands idx out
      in  snd (mapAccumL classify
                 (htlc_candidates keys features htlcs)
                 (zip [0..] outputs))

-- | Classify a single output from a remote commitment tx, removing
--   any matched HTLC from the candidates.
classifyRemoteOutput
  :: TxId
  -> CommitmentKeys
  -> [(HTLC, Script)]
  -> Word32
  -> TxOutput
  -> ([(HTLC, Script)], UnresolvedOutput)
classifyRemoteOutput !txid !keys !cands !idx !out =
  let !op = OutPoint txid idx
      !val = txout_value out
      !(rest, resolution) = case txout_type out of
        OutputToLocal ->
          (cands, Resolved)  -- Remote's to_local; not ours
        OutputToRemote ->
          (cands, Resolved)  -- Our to_remote; resolved by commitment
        OutputLocalAnchor ->
          (cands, Resolved)  -- Remote's anchor
        OutputRemoteAnchor ->
          (cands, AnchorSpend (ck_remote_funding keys))
        OutputOfferedHTLC expiry ->
          -- On remote's commit, their offered = our received.
          -- We can claim with preimage.
          case take_htlc HTLCOffered expiry out cands of
            (Just htlc, cands') ->
              (cands', SpendHTLCPreimageDirect htlc)
            (Nothing, cands') -> (cands', Resolved)
        OutputReceivedHTLC expiry ->
          -- On remote's commit, their received = our offered.
          -- We can claim after timeout.
          case take_htlc HTLCReceived expiry out cands of
            (Just htlc, cands') ->
              (cands', SpendHTLCTimeoutDirect htlc)
            (Nothing, cands') -> (cands', Resolved)
  in (rest, UnresolvedOutput op val resolution)

-- | Classify outputs of a revoked commitment transaction.
--
-- Per BOLT #5: when we discover a revoked commitment, we can
-- claim everything using the revocation key. to_local is spent
-- via revocation, HTLCs are spent via revocation, and we can
-- also optionally sweep to_remote.
classify_revoked_commit_outputs
  :: CommitmentTx
  -- ^ The revoked commitment transaction.
  -> CommitmentKeys
  -- ^ Derived keys for the revoked commitment.
  -> RevocationPubkey
  -- ^ Revocation pubkey (derived from the revealed secret).
  -> ChannelFeatures
  -- ^ Channel feature flags.
  -> [HTLC]
  -- ^ HTLCs in the revoked commitment.
  -> [UnresolvedOutput]
classify_revoked_commit_outputs !commitTx !_keys
    !revpk !_features !_htlcs =
  case commitment_txid commitTx of
    Nothing -> []
    Just !txid ->
      let !outputs = ctx_outputs commitTx
      in  zipWith (classifyRevokedOutput txid revpk)
            [0..] outputs

-- | Classify a single output from a revoked commitment tx.
classifyRevokedOutput
  :: TxId
  -> RevocationPubkey
  -> Word32
  -> TxOutput
  -> UnresolvedOutput
classifyRevokedOutput !txid !revpk !idx !out =
  let !op = OutPoint txid idx
      !val = txout_value out
      !resolution = case txout_type out of
        OutputToLocal ->
          Revoke revpk
        OutputToRemote ->
          Resolved  -- Our funds; resolved by commitment
        OutputLocalAnchor ->
          Resolved  -- Can be swept by anyone after 16 blocks
        OutputRemoteAnchor ->
          Resolved  -- Our anchor
        OutputOfferedHTLC expiry ->
          RevokeHTLC revpk (HTLCOfferedOutput expiry)
        OutputReceivedHTLC expiry ->
          RevokeHTLC revpk (HTLCReceivedOutput expiry)
  in UnresolvedOutput op val resolution

-- preimage extraction ------------------------------------------------

-- | Extract a payment preimage from an offered HTLC witness.
--
-- When the remote party claims an offered HTLC on our local
-- commitment, the witness contains the preimage. The witness
-- stack for a preimage claim is:
--
-- @\<remotehtlcsig\> \<paymentPreimage\> \<witnessScript\>@
--
-- Returns the preimage only if it hashes to the given payment
-- hash. Any other spend of the output, e.g. by revocation, yields
-- 'Nothing'.
extract_preimage_offered
  :: PaymentHash
  -- ^ Payment hash of the HTLC.
  -> Witness
  -- ^ Witness of the input spending the HTLC output.
  -> Maybe PaymentPreimage
extract_preimage_offered !ph (Witness items) =
  case items of
    [_sig, preimageBytes, _script] ->
      check_preimage ph preimageBytes
    _ -> Nothing

-- | Extract a payment preimage from an HTLC-success transaction
--   witness.
--
-- When the remote party uses an HTLC-success tx on their
-- commitment to claim a received HTLC, the witness contains the
-- preimage. The witness stack is:
--
-- @0 \<remotehtlcsig\> \<localhtlcsig\> \<paymentPreimage\>
-- \<witnessScript\>@
--
-- Returns the preimage only if it hashes to the given payment
-- hash.
extract_preimage_htlc_success
  :: PaymentHash
  -- ^ Payment hash of the HTLC.
  -> Witness
  -- ^ Witness of the HTLC-success transaction input.
  -> Maybe PaymentPreimage
extract_preimage_htlc_success !ph (Witness items) =
  case items of
    [_zero, _remoteSig, _localSig, preimageBytes, _script] ->
      check_preimage ph preimageBytes
    _ -> Nothing

-- | Accept a candidate preimage only if it hashes to the payment
--   hash.
check_preimage :: PaymentHash -> BS.ByteString -> Maybe PaymentPreimage
check_preimage !ph !bs
  | SHA256.hash bs == unPaymentHash ph = paymentPreimage bs
  | otherwise = Nothing

-- timeout check ------------------------------------------------------

-- | Check if an HTLC has timed out at the given block height.
--
-- An HTLC has timed out when the current block height is equal
-- to or greater than the HTLC's CLTV expiry.
htlc_timed_out :: Word32 -> HTLC -> Bool
htlc_timed_out !currentHeight !htlc =
  currentHeight >= unCltvExpiry (htlc_cltv_expiry htlc)
{-# INLINE htlc_timed_out #-}

-- internal helpers ---------------------------------------------------

-- | Compute the txid of a commitment transaction.
--
-- Returns 'Nothing' if the commitment has no outputs.
commitment_txid :: CommitmentTx -> Maybe TxId
commitment_txid !tx = fmap BT.txid (commitment_to_tx tx)

-- | Pair each HTLC with the P2WSH script of its commitment output.
htlc_candidates
  :: CommitmentKeys
  -> ChannelFeatures
  -> [HTLC]
  -> [(HTLC, Script)]
htlc_candidates !keys !features = map (\htlc -> (htlc, script htlc))
  where
    script htlc = case htlc_direction htlc of
      HTLCOffered ->
        to_p2wsh $ offered_htlc_script
          (ck_revocation_pubkey keys)
          (ck_remote_htlc keys)
          (ck_local_htlc keys)
          (htlc_payment_hash htlc)
          features
      HTLCReceived ->
        to_p2wsh $ received_htlc_script
          (ck_revocation_pubkey keys)
          (ck_remote_htlc keys)
          (ck_local_htlc keys)
          (htlc_payment_hash htlc)
          (htlc_cltv_expiry htlc)
          features

-- | Find the HTLC for an HTLC output and remove it from the
--   candidates.
--
-- The HTLC must agree with the output on direction, script, amount
-- and CLTV expiry. The offered HTLC script doesn't commit to the
-- expiry and no HTLC script commits to the amount, so HTLCs sharing
-- a payment hash differ only in these. Removing the match ensures
-- no two outputs resolve to the same HTLC.
take_htlc
  :: HTLCDirection
  -> CltvExpiry
  -- ^ Expiry recorded for the output.
  -> TxOutput
  -> [(HTLC, Script)]
  -> (Maybe HTLC, [(HTLC, Script)])
take_htlc !dir !expiry !out !cands =
  case break matches cands of
    (pre, (htlc, _) : post) -> (Just htlc, pre ++ post)
    (_, []) -> (Nothing, cands)
  where
    matches (htlc, script) =
         htlc_direction htlc == dir
      && htlc_cltv_expiry htlc == expiry
      && msatToSat (htlc_amount_msat htlc) == txout_value out
      && script == txout_script out
