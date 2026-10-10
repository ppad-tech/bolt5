{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT5.Spend
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Spending transaction construction for BOLT #5 on-chain
-- transaction handling.
--
-- All functions produce unsigned 'SpendingTx' values. The caller
-- signs (using the sighash metadata provided) and assembles the
-- witness with bolt3's witness functions.

module Lightning.Protocol.BOLT5.Spend (
    -- * Local commitment spends
    spend_to_local
  , spend_htlc_tx
  , spend_htlc_output

    -- * Remote commitment spends
  , spend_remote_htlc_timeout
  , spend_remote_htlc_preimage

    -- * Revoked commitment spends
  , spend_revoked_to_local
  , spend_revoked_htlc
  , spend_revoked_htlc_output

    -- * Anchor spends
  , spend_anchor_owner
  , spend_anchor_anyone
  ) where

import Bitcoin.Prim.Tx (OutPoint, Tx(..), TxIn(..), TxOut(..))
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word32, Word64)
import Lightning.Protocol.BOLT1 (PaymentHash, Satoshi)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT5.Types

-- local commitment spends --------------------------------------------

-- | Spend the to_local output of our local commitment tx.
--
--   The input's nSequence is the to_self_delay, so the transaction
--   is valid only once the delay has passed. The caller signs with
--   the local delayed privkey and uses bolt3's
--   'to_local_witness_spend'.
--
--   Returns 'Nothing' if the fee would exceed the output value, or
--   leave an output below the destination's dust threshold.
spend_to_local
  :: OutPoint
  -- ^ Outpoint of the to_local output.
  -> Satoshi
  -- ^ Value of the to_local output.
  -> RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_to_local !op !value !revpk delay@(ToSelfDelay d) !delayedpk
    !dest !feerate =
  sweep op value (to_local_script revpk delay delayedpk)
    to_local_penalty_input_weight (fromIntegral d) 0 dest feerate

-- | Build the HTLC-timeout (for an offered HTLC) or HTLC-success
--   (for a received HTLC) transaction spending an HTLC output of
--   our local commitment, with bolt3's 'build_htlc_tx'.
--
--   The sighash type is that of the local signature, which is
--   always @SIGHASH_ALL@. The remote's @htlc_signature@ uses bolt3's
--   'remote_htlc_sighash' (@SIGHASH_SINGLE|SIGHASH_ANYONECANPAY@
--   under 'Anchors'). The witness is bolt3's 'htlc_timeout_witness'
--   or 'htlc_success_witness'.
--
--   Under 'Anchors' the transaction pays no fee; it must be
--   combined with other inputs before broadcast.
--
--   Returns 'Nothing' if bolt3 can't build the transaction (the fee
--   exceeds the HTLC amount).
spend_htlc_tx
  :: HTLCContext
  -> CommitmentKeys
  -- ^ Keys for our local commitment (for the witness script).
  -> Maybe SpendingTx
spend_htlc_tx !ctx !keys = do
  htx <- build_htlc_tx ctx
  let !h = hc_htlc ctx
  pure SpendingTx
    { stx_tx           = htlc_to_tx htx
    , stx_input_script = htlc_script keys (hc_format ctx) h
    , stx_input_value  = BOLT1.msat_to_sat (htlc_amount_msat h)
    , stx_sighash_type = SIGHASH_ALL
    }

-- | Spend the output of an HTLC-timeout or HTLC-success
--   transaction after the CSV delay.
--
--   The output uses the to_local script, so this is
--   'spend_to_local'. The caller signs with the local delayed
--   privkey and uses bolt3's 'to_local_witness_spend'.
spend_htlc_output
  :: OutPoint
  -- ^ Outpoint of the second-stage output.
  -> Satoshi
  -- ^ Value of the second-stage output.
  -> RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_htlc_output = spend_to_local

-- remote commitment spends -------------------------------------------

-- | Spend an HTLC we offered directly, after it times out, from the
--   remote commitment (where it is a received HTLC).
--
--   The locktime is the HTLC's CLTV expiry. The caller signs with
--   our HTLC privkey and uses bolt3's
--   'received_htlc_witness_timeout'.
--
--   Returns 'Nothing' if the fee would exceed the output value, or
--   leave an output below the destination's dust threshold.
spend_remote_htlc_timeout
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLC
  -- ^ The HTLC being spent.
  -> CommitmentKeys
  -- ^ Keys for the remote commitment.
  -> CommitmentFormat
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_remote_htlc_timeout !op !value !htlc !keys !fmt !dest !feerate =
  let expiry@(CltvExpiry e) = htlc_cltv_expiry htlc
      !script = received_htlc_script (ck_revocation_pubkey keys)
        (ck_remote_htlc keys) (ck_local_htlc keys)
        (htlc_payment_hash htlc) expiry fmt
  in  sweep op value script
        (htlc_output_type_weight fmt (HTLCReceivedOutput expiry))
        (remote_htlc_sequence fmt) e dest feerate

-- | Spend an HTLC we received directly with the preimage, from the
--   remote commitment (where it is an offered HTLC).
--
--   The caller signs with our HTLC privkey and uses bolt3's
--   'offered_htlc_witness_preimage'.
--
--   Returns 'Nothing' if the fee would exceed the output value, or
--   leave an output below the destination's dust threshold.
spend_remote_htlc_preimage
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLC
  -- ^ The HTLC being spent.
  -> CommitmentKeys
  -- ^ Keys for the remote commitment.
  -> CommitmentFormat
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_remote_htlc_preimage !op !value !htlc !keys !fmt !dest !feerate =
  let !expiry = htlc_cltv_expiry htlc
      !script = offered_htlc_script (ck_revocation_pubkey keys)
        (ck_remote_htlc keys) (ck_local_htlc keys)
        (htlc_payment_hash htlc) fmt
  in  sweep op value script
        (htlc_output_type_weight fmt (HTLCOfferedOutput expiry))
        (remote_htlc_sequence fmt) 0 dest feerate

-- revoked commitment spends ------------------------------------------

-- | Spend a revoked to_local output with the revocation key.
--
--   The caller signs with the revocation privkey and uses bolt3's
--   'to_local_witness_revoke'.
--
--   Returns 'Nothing' if the fee would exceed the output value, or
--   leave an output below the destination's dust threshold.
spend_revoked_to_local
  :: OutPoint
  -- ^ Outpoint of the to_local output.
  -> Satoshi
  -- ^ Value of the to_local output.
  -> RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_revoked_to_local !op !value !revpk !delay !delayedpk !dest
    !feerate =
  sweep op value (to_local_script revpk delay delayedpk)
    to_local_penalty_input_weight 0xFFFFFFFF 0 dest feerate

-- | Spend a revoked HTLC output with the revocation key.
--
--   The caller signs with the revocation privkey and uses bolt3's
--   'offered_htlc_witness_revoke' or 'received_htlc_witness_revoke',
--   according to the output type.
--
--   Returns 'Nothing' if the fee would exceed the output value, or
--   leave an output below the destination's dust threshold.
spend_revoked_htlc
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLCOutputType
  -- ^ Whether offered or received HTLC.
  -> RevocationPubkey
  -> CommitmentKeys
  -> CommitmentFormat
  -> PaymentHash
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_revoked_htlc !op !value !htype !revpk !keys !fmt !ph !dest
    !feerate =
  let !remote = ck_remote_htlc keys
      !local = ck_local_htlc keys
      !script = case htype of
        HTLCOfferedOutput _ ->
          offered_htlc_script revpk remote local ph fmt
        HTLCReceivedOutput expiry ->
          received_htlc_script revpk remote local ph expiry fmt
  in  sweep op value script (htlc_output_type_weight fmt htype)
        0xFFFFFFFF 0 dest feerate

-- | Spend the output of a revoked HTLC-timeout or HTLC-success
--   transaction with the revocation key.
--
--   The output uses the to_local script, so this is
--   'spend_revoked_to_local'. The caller signs with the revocation
--   privkey and uses bolt3's 'to_local_witness_revoke'.
spend_revoked_htlc_output
  :: OutPoint
  -- ^ Outpoint of the second-stage output.
  -> Satoshi
  -- ^ Value of the second-stage output.
  -> RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_revoked_htlc_output = spend_revoked_to_local

-- anchor spends ------------------------------------------------------

-- | Spend an anchor output as its owner (immediately).
--
--   The transaction pays no fee and has a single input: it must be
--   combined with other inputs before broadcast. The caller signs
--   with the funding privkey and uses bolt3's
--   'anchor_witness_owner'.
spend_anchor_owner
  :: OutPoint
  -- ^ Outpoint of the anchor output.
  -> Satoshi
  -- ^ Value of the anchor output (330 sats).
  -> FundingPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> SpendingTx
spend_anchor_owner !op !value !fundpk !dest =
  SpendingTx (mk_spending_tx op 0xFFFFFFFE dest value 0)
    (anchor_script fundpk) value SIGHASH_ALL

-- | Spend an anchor output as anyone (16 blocks after the
--   commitment confirms).
--
--   As 'spend_anchor_owner', the transaction pays no fee. The
--   witness is bolt3's 'anchor_witness_anyone', which needs no
--   signature.
spend_anchor_anyone
  :: OutPoint
  -- ^ Outpoint of the anchor output.
  -> Satoshi
  -- ^ Value of the anchor output (330 sats).
  -> FundingPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> SpendingTx
spend_anchor_anyone !op !value !fundpk !dest =
  SpendingTx (mk_spending_tx op 16 dest value 0)
    (anchor_script fundpk) value SIGHASH_ALL

-- internal helpers ---------------------------------------------------

-- | A single-input, single-output sweep paying the input value, less
--   the fee for the given input weight, to the destination.
sweep
  :: OutPoint
  -> Satoshi      -- ^ input value
  -> Script       -- ^ input witness script
  -> Word64       -- ^ input weight
  -> Word32       -- ^ input nSequence
  -> Word32       -- ^ locktime
  -> Script       -- ^ destination scriptPubKey
  -> FeeratePerKw
  -> Maybe SpendingTx
sweep !op !value !script !weight !sq !locktime !dest !feerate = do
  let !fee = spending_fee feerate (weight + penalty_tx_base_weight)
  out <- sweep_value value fee dest
  pure (SpendingTx (mk_spending_tx op sq dest out locktime) script
          value SIGHASH_ALL)

-- | Value of a sweep's single output: the input value less the fee.
--
--   'Nothing' if the fee exceeds the input value, or if the output
--   would be below the dust threshold of its script.
sweep_value :: Satoshi -> Satoshi -> Script -> Maybe Satoshi
sweep_value !value !fee (Script dest) = do
  out <- BOLT1.sub_sat value fee
  if   BOLT1.un_satoshi out < dust_threshold dest
  then Nothing
  else Just out

-- | Dust threshold, in satoshis, for an output paying to a script.
--
--   As Bitcoin Core computes it for spendable outputs at the default
--   dust relay feerate of 3 sat/vbyte: three times the size of the
--   output plus that of an input spending it (67 vbytes for a
--   witness program, 148 otherwise). This gives 546 sats for P2PKH,
--   540 for P2SH, 294 for P2WPKH, and 330 for P2WSH and P2TR.
dust_threshold :: BS.ByteString -> Word64
dust_threshold s =
  let !len = fromIntegral (BS.length s) :: Word64
      !len_size
        | len < 0xfd = 1
        | len <= 0xffff = 3
        | otherwise = 5
      !spend_size = if is_witness_program s then 67 else 148
  in  3 * (8 + len_size + len + spend_size)

-- | Whether a script is a segwit witness program: a version opcode
--   (OP_0 or OP_1 to OP_16) followed by a single 2-40 byte push.
is_witness_program :: BS.ByteString -> Bool
is_witness_program s = case BS.unpack (BS.take 2 s) of
  [version, push_len] ->
       (version == 0x00 || (version >= 0x51 && version <= 0x60))
    && push_len >= 2 && push_len <= 40
    && fromIntegral push_len + 2 == BS.length s
  _ -> False

-- | The nSequence of a direct HTLC spend from the remote commitment:
--   1 under 'Anchors' (for the HTLC scripts' CSV), 0 otherwise.
remote_htlc_sequence :: CommitmentFormat -> Word32
remote_htlc_sequence fmt = case fmt of
  StaticRemotekey -> 0
  Anchors         -> 1

-- | The witness script of an HTLC output of a commitment with the
--   given keys.
htlc_script :: CommitmentKeys -> CommitmentFormat -> HTLC -> Script
htlc_script !keys !fmt !h =
  let !revpk = ck_revocation_pubkey keys
      !remote = ck_remote_htlc keys
      !local = ck_local_htlc keys
      !ph = htlc_payment_hash h
  in  case htlc_direction h of
        HTLCOffered  -> offered_htlc_script revpk remote local ph fmt
        HTLCReceived ->
          received_htlc_script revpk remote local ph
            (htlc_cltv_expiry h) fmt

-- | Build a version 2, single-input, single-output transaction.
mk_spending_tx
  :: OutPoint     -- ^ input outpoint
  -> Word32       -- ^ input nSequence
  -> Script       -- ^ output scriptPubKey
  -> Satoshi      -- ^ output value
  -> Word32       -- ^ locktime
  -> Tx
mk_spending_tx !op !sq (Script dest) !value !locktime = Tx
  { tx_version   = 2
  , tx_inputs    = TxIn
      { txin_prevout    = op
      , txin_script_sig = BS.empty
      , txin_sequence   = sq
      } :| []
  , tx_outputs   = TxOut
      { txout_value         = BOLT1.un_satoshi value
      , txout_script_pubkey = dest
      } :| []
  , tx_witnesses = []
  , tx_locktime  = locktime
  }
