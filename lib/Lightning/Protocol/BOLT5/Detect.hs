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
import Data.Word (Word32)
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT5.Types

-- close identification -----------------------------------------------

-- | Identify the type of channel close from a transaction that
--   spends the funding output.
--
-- Compares the on-chain transaction bytes against the known
-- local and remote commitment transaction serializations
-- (stripped/unsigned) to determine whether it's a local or
-- remote commitment close.
--
-- Returns 'Nothing' if the transaction doesn't match either
-- commitment. Mutual close and revoked commitment detection
-- require additional checks by the caller (e.g. comparing
-- closing tx format, checking a secret store for older
-- commitment numbers).
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
    let !localBytes = encode_tx_for_signing localCommitTx
        !remoteBytes = encode_tx_for_signing remoteCommitTx
    in  if localBytes == Just onChainBytes
        then Just LocalCommitClose
        else if remoteBytes == Just onChainBytes
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
      in  zipWith (classifyLocalOutput txid revpk delayedpk
                     delay features keys htlcs)
            [0..] outputs

-- | Classify a single output from a local commitment tx.
classifyLocalOutput
  :: TxId
  -> RevocationPubkey
  -> LocalDelayedPubkey
  -> ToSelfDelay
  -> ChannelFeatures
  -> CommitmentKeys
  -> [HTLC]
  -> Word32
  -> TxOutput
  -> UnresolvedOutput
classifyLocalOutput !txid !revpk !delayedpk !delay
    !features !keys !htlcs !idx !out =
  let !op = OutPoint txid idx
      !val = txout_value out
      !resolution = case txout_type out of
        OutputToLocal ->
          SpendToLocal delay revpk delayedpk
        OutputToRemote ->
          Resolved
        OutputLocalAnchor ->
          AnchorSpend (ck_local_funding keys)
        OutputRemoteAnchor ->
          Resolved
        OutputOfferedHTLC _expiry ->
          case findHTLC HTLCOffered
                 (txout_script out) keys features htlcs of
            Just htlc ->
              SpendHTLCTimeout htlc keys features
            Nothing -> Resolved
        OutputReceivedHTLC _expiry ->
          case findHTLC HTLCReceived
                 (txout_script out) keys features htlcs of
            Just htlc ->
              SpendHTLCSuccess htlc keys features
            Nothing -> Resolved
  in UnresolvedOutput op val resolution

-- | Classify outputs of the remote commitment transaction.
--
-- Per BOLT #5: when we discover the remote commitment on chain,
-- there are no CSV delays on our outputs. We can spend offered
-- HTLCs directly after timeout, and received HTLCs directly
-- with the preimage.
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
      in  zipWith
            (classifyRemoteOutput txid features keys htlcs)
            [0..] outputs

-- | Classify a single output from a remote commitment tx.
classifyRemoteOutput
  :: TxId
  -> ChannelFeatures
  -> CommitmentKeys
  -> [HTLC]
  -> Word32
  -> TxOutput
  -> UnresolvedOutput
classifyRemoteOutput !txid !features !keys
    !htlcs !idx !out =
  let !op = OutPoint txid idx
      !val = txout_value out
      !resolution = case txout_type out of
        OutputToLocal ->
          Resolved  -- Remote's to_local; not ours
        OutputToRemote ->
          Resolved  -- Our to_remote; resolved by commitment
        OutputLocalAnchor ->
          Resolved  -- Remote's anchor
        OutputRemoteAnchor ->
          AnchorSpend (ck_remote_funding keys)
        OutputOfferedHTLC _expiry ->
          -- On remote's commit, their offered = our received.
          -- We can claim with preimage.
          case findHTLC HTLCOffered
                 (txout_script out) keys features htlcs of
            Just htlc ->
              SpendHTLCPreimageDirect htlc
            Nothing -> Resolved
        OutputReceivedHTLC _expiry ->
          -- On remote's commit, their received = our offered.
          -- We can claim after timeout.
          case findHTLC HTLCReceived
                 (txout_script out) keys features htlcs of
            Just htlc ->
              SpendHTLCTimeoutDirect htlc
            Nothing -> Resolved
  in UnresolvedOutput op val resolution

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
-- @\<remotehtlcsig\> \<paymentPreimage\>@
--
-- The preimage is the second item (32 bytes) and must hash to
-- the expected payment hash.
extract_preimage_offered :: Witness -> Maybe PaymentPreimage
extract_preimage_offered (Witness items) =
  case items of
    [_sig, preimageBytes]
      | BS.length preimageBytes == 32 ->
          paymentPreimage preimageBytes
    _ -> Nothing

-- | Extract a payment preimage from an HTLC-success transaction
--   witness.
--
-- When the remote party uses an HTLC-success tx on their
-- commitment to claim a received HTLC, the witness contains the
-- preimage. The witness stack is:
--
-- @0 \<remotehtlcsig\> \<localhtlcsig\> \<paymentPreimage\>@
--
-- The preimage is the fourth item (32 bytes).
extract_preimage_htlc_success
  :: Witness -> Maybe PaymentPreimage
extract_preimage_htlc_success (Witness items) =
  case items of
    [_zero, _remoteSig, _localSig, preimageBytes]
      | BS.length preimageBytes == 32 ->
          paymentPreimage preimageBytes
    _ -> Nothing

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

-- | Find an HTLC matching a given script in the output list.
findHTLC
  :: HTLCDirection
  -> Script
  -> CommitmentKeys
  -> ChannelFeatures
  -> [HTLC]
  -> Maybe HTLC
findHTLC !dir !targetScript !keys !features =
  go
  where
    go [] = Nothing
    go (htlc:rest)
      | htlc_direction htlc == dir
      , htlcScript htlc == targetScript = Just htlc
      | otherwise = go rest

    htlcScript htlc = case dir of
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
