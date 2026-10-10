{-# OPTIONS_HADDOCK hide #-}
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

import Bitcoin.Prim.Tx (OutPoint(..), TxId, Witness(..))
import qualified Bitcoin.Prim.Tx as BT
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Lightning.Protocol.BOLT1 (PaymentHash, PaymentPreimage)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT5.Types

-- close identification -----------------------------------------------

-- | Identify the type of channel close from a transaction that
--   spends the funding output.
--
--   Parses the on-chain transaction and compares its txid with the
--   txids of the known local and remote commitment transactions.
--   Comparing txids rather than bytes means the signed (segwit)
--   serialization found on chain matches the unsigned commitment.
--
--   Returns 'Nothing' if the bytes don't parse as a transaction or
--   the transaction matches neither commitment. Mutual close and
--   revoked commitment detection are left to the caller (e.g.
--   comparing with the closing transaction, or checking a secret
--   store for older commitment numbers).
identify_close
  :: CommitmentTx
  -- ^ Our local commitment tx.
  -> CommitmentTx
  -- ^ The remote commitment tx (current).
  -> BS.ByteString
  -- ^ Raw serialized transaction found on chain.
  -> Maybe CloseType
identify_close !local !remote !bytes = do
  !tx <- BT.from_bytes bytes
  let !txid = BT.txid tx
  if   commitment_txid local == txid
  then Just LocalCommitClose
  else if commitment_txid remote == txid
  then Just RemoteCommitClose
  else Nothing

-- output classification ----------------------------------------------

-- | Classify the outputs of our local commitment transaction.
--
--   Per BOLT #5: when our local commitment is on chain, we must
--   resolve each output. to_local requires a CSV-delayed spend,
--   to_remote is resolved by the commitment itself, HTLC outputs
--   need second-stage transactions, and our anchor can be spent
--   immediately.
--
--   Each HTLC output's HTLC is read from its bolt3 'OutputHTLC', so
--   the commitment must be the one bolt3 built for this state.
classify_local_commit_outputs
  :: CommitmentTx
  -- ^ Our local commitment transaction.
  -> CommitmentKeys
  -- ^ Derived keys for this commitment.
  -> ToSelfDelay
  -- ^ Remote's to_self_delay (CSV delay for our outputs).
  -> CommitmentFormat
  -> [UnresolvedOutput]
classify_local_commit_outputs !tx !keys !delay !fmt =
    classify_outputs resolve tx
  where
    resolve t = case t of
      OutputToLocal ->
        SpendToLocal delay (ck_revocation_pubkey keys)
          (ck_local_delayed keys)
      OutputToRemote -> Resolved
      OutputLocalAnchor -> AnchorSpend (ck_local_funding keys)
      OutputRemoteAnchor -> Resolved
      OutputHTLC h -> case htlc_direction h of
        HTLCOffered  -> SpendHTLCTimeout h keys fmt
        HTLCReceived -> SpendHTLCSuccess h keys fmt

-- | Classify the outputs of the remote commitment transaction.
--
--   Per BOLT #5: when the remote commitment is on chain, there are
--   no CSV delays on our outputs. We can spend HTLCs we offered
--   directly after timeout, and HTLCs we received directly with the
--   preimage.
--
--   The commitment, its keys and its HTLCs are from the remote's
--   point of view (as bolt3 builds the remote's commitment): an
--   HTLC the remote offered is one we received.
classify_remote_commit_outputs
  :: CommitmentTx
  -- ^ The remote commitment transaction.
  -> CommitmentKeys
  -- ^ Derived keys for this commitment (from the remote's point
  --   of view, so local and remote are swapped).
  -> [UnresolvedOutput]
classify_remote_commit_outputs !tx !keys =
    classify_outputs resolve tx
  where
    resolve t = case t of
      OutputToLocal -> Resolved       -- the remote's to_local
      OutputToRemote -> Resolved      -- ours; resolved by the commitment
      OutputLocalAnchor -> Resolved   -- the remote's anchor
      OutputRemoteAnchor -> AnchorSpend (ck_remote_funding keys)
      OutputHTLC h -> case htlc_direction h of
        HTLCOffered  -> SpendHTLCPreimageDirect h  -- we received it
        HTLCReceived -> SpendHTLCTimeoutDirect h   -- we offered it

-- | Classify the outputs of a revoked commitment transaction.
--
--   Per BOLT #5: when a revoked commitment is on chain, we can
--   claim to_local and every HTLC output with the revocation key.
--   to_remote (ours) and the anchors are marked 'Resolved'.
classify_revoked_commit_outputs
  :: CommitmentTx
  -- ^ The revoked commitment transaction.
  -> RevocationPubkey
  -- ^ Revocation pubkey (derived from the revealed secret).
  -> [UnresolvedOutput]
classify_revoked_commit_outputs !tx !revpk =
    classify_outputs resolve tx
  where
    resolve t = case t of
      OutputToLocal -> Revoke revpk
      OutputToRemote -> Resolved
      OutputLocalAnchor -> Resolved
      OutputRemoteAnchor -> Resolved
      OutputHTLC h -> RevokeHTLC revpk $ case htlc_direction h of
        HTLCOffered  -> HTLCOfferedOutput (htlc_cltv_expiry h)
        HTLCReceived -> HTLCReceivedOutput (htlc_cltv_expiry h)

-- | Pair each commitment output with its outpoint, value and
--   resolution.
classify_outputs
  :: (OutputType -> OutputResolution)
  -> CommitmentTx
  -> [UnresolvedOutput]
classify_outputs resolve tx =
    zipWith classify [0..] (NE.toList (ctx_outputs tx))
  where
    !txid = commitment_txid tx
    classify :: Word32 -> CommitmentOutput -> UnresolvedOutput
    classify !idx !out = UnresolvedOutput
      { uo_outpoint = OutPoint txid idx
      , uo_value    = co_value out
      , uo_type     = resolve (co_type out)
      }

-- preimage extraction ------------------------------------------------

-- | Extract a payment preimage from the witness of an input
--   spending an offered HTLC output.
--
--   When the remote party claims an offered HTLC on our local
--   commitment, the witness (see bolt3's
--   'offered_htlc_witness_preimage') is:
--
--   @\<remotehtlcsig\> \<payment_preimage\> \<witness_script\>@
--
--   Returns the preimage only if it hashes to the given payment
--   hash. Any other spend of the output, e.g. by revocation, yields
--   'Nothing'.
extract_preimage_offered
  :: PaymentHash
  -- ^ Payment hash of the HTLC.
  -> Witness
  -- ^ Witness of the input spending the HTLC output.
  -> Maybe PaymentPreimage
extract_preimage_offered !ph (Witness items) = case items of
  [_sig, preimage, _script] -> check_preimage ph preimage
  _ -> Nothing

-- | Extract a payment preimage from the witness of an HTLC-success
--   transaction's input.
--
--   When the remote party claims an HTLC it received on its
--   commitment with an HTLC-success transaction, the witness (see
--   bolt3's 'htlc_success_witness') is:
--
--   @0 \<remotehtlcsig\> \<localhtlcsig\> \<payment_preimage\>
--   \<witness_script\>@
--
--   Returns the preimage only if it hashes to the given payment
--   hash.
extract_preimage_htlc_success
  :: PaymentHash
  -- ^ Payment hash of the HTLC.
  -> Witness
  -- ^ Witness of the HTLC-success transaction input.
  -> Maybe PaymentPreimage
extract_preimage_htlc_success !ph (Witness items) = case items of
  [_zero, _remote_sig, _local_sig, preimage, _script] ->
    check_preimage ph preimage
  _ -> Nothing

-- | Accept a candidate preimage only if it hashes to the payment
--   hash.
check_preimage :: PaymentHash -> BS.ByteString -> Maybe PaymentPreimage
check_preimage !ph !bs
  | SHA256.hash bs == BOLT1.un_payment_hash ph = BOLT1.payment_preimage bs
  | otherwise = Nothing

-- timeout check ------------------------------------------------------

-- | Whether an HTLC has timed out at the given block height: whether
--   the height is at least the HTLC's CLTV expiry.
--
--   >>> let Just amt = BOLT1.milli_satoshi 1000000
--   >>> let Just ph = BOLT1.payment_hash (BS.replicate 32 0xaa)
--   >>> let htlc = HTLC HTLCOffered amt ph (CltvExpiry 800000)
--   >>> htlc_timed_out 799999 htlc
--   False
--   >>> htlc_timed_out 800000 htlc
--   True
htlc_timed_out :: Word32 -> HTLC -> Bool
htlc_timed_out !height !htlc =
  let CltvExpiry expiry = htlc_cltv_expiry htlc
  in  height >= expiry
{-# INLINE htlc_timed_out #-}

-- internal helpers ---------------------------------------------------

-- | The txid of a commitment transaction.
commitment_txid :: CommitmentTx -> TxId
commitment_txid = BT.txid . commitment_to_tx
{-# INLINE commitment_txid #-}
