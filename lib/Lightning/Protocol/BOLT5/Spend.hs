{-# OPTIONS_HADDOCK prune #-}
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
-- is responsible for signing (using the sighash metadata
-- provided) and assembling final witnesses via bolt3 witness
-- constructors.

module Lightning.Protocol.BOLT5.Spend (
    -- * Local commitment spends
    spend_to_local
  , spend_htlc_timeout
  , spend_htlc_success
  , spend_htlc_output

    -- * Remote commitment spends
  , spend_remote_htlc_timeout
  , spend_remote_htlc_preimage

    -- * Revoked commitment spends
  , spend_revoked_to_local
  , spend_revoked_htlc
  , spend_revoked_htlc_output
  , spend_revoked_batch

    -- * Anchor spends
  , spend_anchor_owner
  , spend_anchor_anyone
  ) where

import Bitcoin.Prim.Tx (TxOut(..))
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT3 hiding
  (txout_value, txout_script)
import Lightning.Protocol.BOLT5.Types

-- local commitment spends --------------------------------------------

-- | Spend the to_local output of our local commitment tx.
--
-- Requires waiting for the CSV delay (to_self_delay) before
-- broadcasting. The caller signs with the local delayed privkey
-- and uses 'to_local_witness_spend' from bolt3.
--
-- Returns 'Nothing' if the fee would exceed the output value.
--
-- The input nSequence is set to the to_self_delay value.
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
spend_to_local !op !value !revpk !delay !delayedpk
    !destScript !feerate =
  let !witnessScript =
        to_local_script revpk delay delayedpk
      !weight = to_local_penalty_input_weight
              + penalty_tx_base_weight
      !fee = spending_fee feerate weight
  in if unSatoshi fee >= unSatoshi value
     then Nothing
     else
       let !outputValue =
             Satoshi (unSatoshi value - unSatoshi fee)
           !tx = mk_spending_tx op
                   (fromIntegral (unToSelfDelay delay))
                   destScript outputValue 0
       in Just (SpendingTx tx witnessScript value
                  SIGHASH_ALL)

-- | Construct an HTLC-timeout second-stage transaction.
--
-- Used when we offered an HTLC on our local commitment and it
-- has timed out. The bolt3 'build_htlc_timeout_tx' function
-- constructs the HTLC-timeout tx; this wraps it as a
-- 'SpendingTx' with the witness script and sighash metadata.
spend_htlc_timeout
  :: HTLCContext
  -> CommitmentKeys
  -- ^ Full commitment keys (needed for witness script).
  -> SpendingTx
spend_htlc_timeout !ctx !keys =
  let !htlcTx = build_htlc_timeout_tx ctx
      !htlc = hc_htlc ctx
      !features = hc_features ctx
      !witnessScript = offered_htlc_script
        (ck_revocation_pubkey keys)
        (ck_remote_htlc keys)
        (ck_local_htlc keys)
        (htlc_payment_hash htlc)
        features
      !inputValue =
        msatToSat (htlc_amount_msat htlc)
      !sighashType = if has_anchors features
        then SIGHASH_SINGLE_ANYONECANPAY
        else SIGHASH_ALL
      !tx = htlc_tx_to_tx htlcTx
  in SpendingTx tx witnessScript inputValue sighashType

-- | Construct an HTLC-success second-stage transaction.
--
-- Used when we received an HTLC on our local commitment and
-- have the preimage. The bolt3 'build_htlc_success_tx' function
-- constructs the HTLC-success tx; this wraps it as a
-- 'SpendingTx'.
spend_htlc_success
  :: HTLCContext
  -> CommitmentKeys
  -- ^ Full commitment keys (needed for witness script).
  -> SpendingTx
spend_htlc_success !ctx !keys =
  let !htlcTx = build_htlc_success_tx ctx
      !htlc = hc_htlc ctx
      !features = hc_features ctx
      !witnessScript = received_htlc_script
        (ck_revocation_pubkey keys)
        (ck_remote_htlc keys)
        (ck_local_htlc keys)
        (htlc_payment_hash htlc)
        (htlc_cltv_expiry htlc)
        features
      !inputValue =
        msatToSat (htlc_amount_msat htlc)
      !sighashType = if has_anchors features
        then SIGHASH_SINGLE_ANYONECANPAY
        else SIGHASH_ALL
      !tx = htlc_tx_to_tx htlcTx
  in SpendingTx tx witnessScript inputValue sighashType

-- | Spend a second-stage HTLC output (HTLC-timeout or
--   HTLC-success output) after the CSV delay.
--
-- The output of an HTLC-timeout or HTLC-success tx uses the
-- same to_local script. The caller signs with the local
-- delayed privkey and uses 'htlc_output_witness_spend'.
--
-- Returns 'Nothing' if the fee would exceed the output value.
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

-- | Spend an offered HTLC directly after timeout on the remote
--   commitment.
--
-- On the remote commitment, their received HTLCs (our offered)
-- have timed out and we can sweep them directly.
--
-- Returns 'Nothing' if the fee would exceed the output value.
spend_remote_htlc_timeout
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLC
  -- ^ The HTLC being spent.
  -> CommitmentKeys
  -- ^ Keys for the remote commitment.
  -> ChannelFeatures
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_remote_htlc_timeout !op !value !htlc !keys
    !features !destScript !feerate =
  let !witnessScript = received_htlc_script
        (ck_revocation_pubkey keys)
        (ck_remote_htlc keys)
        (ck_local_htlc keys)
        (htlc_payment_hash htlc)
        (htlc_cltv_expiry htlc)
        features
      !weight = accepted_htlc_penalty_input_weight
              + penalty_tx_base_weight
      !fee = spending_fee feerate weight
  in if unSatoshi fee >= unSatoshi value
     then Nothing
     else
       let !outputValue =
             Satoshi (unSatoshi value - unSatoshi fee)
           !locktime =
             unCltvExpiry (htlc_cltv_expiry htlc)
           !seqNo =
             if has_anchors features then 1 else 0
           !tx = mk_spending_tx op seqNo destScript
                   outputValue locktime
       in Just (SpendingTx tx witnessScript value
                  SIGHASH_ALL)

-- | Spend a received HTLC directly with preimage on the remote
--   commitment.
--
-- On the remote commitment, their offered HTLCs (our received)
-- can be claimed with the payment preimage.
--
-- Returns 'Nothing' if the fee would exceed the output value.
spend_remote_htlc_preimage
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLC
  -- ^ The HTLC being spent.
  -> CommitmentKeys
  -- ^ Keys for the remote commitment.
  -> ChannelFeatures
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_remote_htlc_preimage !op !value !htlc !keys
    !features !destScript !feerate =
  let !witnessScript = offered_htlc_script
        (ck_revocation_pubkey keys)
        (ck_remote_htlc keys)
        (ck_local_htlc keys)
        (htlc_payment_hash htlc)
        features
      !weight = offered_htlc_penalty_input_weight
              + penalty_tx_base_weight
      !fee = spending_fee feerate weight
  in if unSatoshi fee >= unSatoshi value
     then Nothing
     else
       let !outputValue =
             Satoshi (unSatoshi value - unSatoshi fee)
           !seqNo =
             if has_anchors features then 1 else 0
           !tx = mk_spending_tx op seqNo destScript
                   outputValue 0
       in Just (SpendingTx tx witnessScript value
                  SIGHASH_ALL)

-- revoked commitment spends ------------------------------------------

-- | Spend a revoked to_local output using the revocation key.
--
-- The caller signs with the revocation privkey and uses
-- 'to_local_witness_revoke' from bolt3.
--
-- Returns 'Nothing' if the fee would exceed the output value.
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
spend_revoked_to_local !op !value !revpk !delay
    !delayedpk !destScript !feerate =
  let !witnessScript =
        to_local_script revpk delay delayedpk
      !weight = to_local_penalty_input_weight
              + penalty_tx_base_weight
      !fee = spending_fee feerate weight
  in if unSatoshi fee >= unSatoshi value
     then Nothing
     else
       let !outputValue =
             Satoshi (unSatoshi value - unSatoshi fee)
           !tx = mk_spending_tx op 0xFFFFFFFF destScript
                   outputValue 0
       in Just (SpendingTx tx witnessScript value
                  SIGHASH_ALL)

-- | Spend a revoked HTLC output using the revocation key.
--
-- The caller signs with the revocation privkey and uses
-- 'offered_htlc_witness_revoke' or
-- 'received_htlc_witness_revoke' from bolt3, depending on
-- the output type.
--
-- Returns 'Nothing' if the fee would exceed the output
-- value.
spend_revoked_htlc
  :: OutPoint
  -- ^ Outpoint of the HTLC output.
  -> Satoshi
  -- ^ Value of the HTLC output.
  -> HTLCOutputType
  -- ^ Whether offered or received HTLC.
  -> RevocationPubkey
  -> CommitmentKeys
  -> ChannelFeatures
  -> PaymentHash
  -> Script
  -- ^ Destination scriptPubKey.
  -> FeeratePerKw
  -> Maybe SpendingTx
spend_revoked_htlc !op !value !htype !revpk !keys
    !features !ph !destScript !feerate =
  case htype of
    HTLCOfferedOutput _ ->
      let !witnessScript = offered_htlc_script
            revpk
            (ck_remote_htlc keys)
            (ck_local_htlc keys)
            ph
            features
          !weight = offered_htlc_penalty_input_weight
                  + penalty_tx_base_weight
          !fee = spending_fee feerate weight
      in if unSatoshi fee >= unSatoshi value
         then Nothing
         else
           let !outputValue =
                 Satoshi
                   (unSatoshi value - unSatoshi fee)
               !tx = mk_spending_tx op 0xFFFFFFFF
                       destScript outputValue 0
           in Just (SpendingTx tx witnessScript value
                      SIGHASH_ALL)
    HTLCReceivedOutput expiry ->
      let !witnessScript = received_htlc_script
            revpk
            (ck_remote_htlc keys)
            (ck_local_htlc keys)
            ph
            expiry
            features
          !weight = accepted_htlc_penalty_input_weight
                  + penalty_tx_base_weight
          !fee = spending_fee feerate weight
      in if unSatoshi fee >= unSatoshi value
         then Nothing
         else
           let !outputValue =
                 Satoshi
                   (unSatoshi value - unSatoshi fee)
               !tx = mk_spending_tx op 0xFFFFFFFF
                       destScript outputValue 0
           in Just (SpendingTx tx witnessScript value
                      SIGHASH_ALL)

-- | Spend a revoked second-stage HTLC output (HTLC-timeout or
--   HTLC-success output) using the revocation key.
--
-- The output of a revoked HTLC-timeout/success tx uses the
-- to_local script. The caller signs with the revocation privkey
-- and uses 'htlc_output_witness_revoke'.
--
-- Returns 'Nothing' if the fee would exceed the output value.
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
spend_revoked_htlc_output !op !value !revpk !delay
    !delayedpk !destScript !feerate =
  let !witnessScript =
        to_local_script revpk delay delayedpk
      !weight = to_local_penalty_input_weight
              + penalty_tx_base_weight
      !fee = spending_fee feerate weight
  in if unSatoshi fee >= unSatoshi value
     then Nothing
     else
       let !outputValue =
             Satoshi (unSatoshi value - unSatoshi fee)
           !tx = mk_spending_tx op 0xFFFFFFFF destScript
                   outputValue 0
       in Just (SpendingTx tx witnessScript value
                  SIGHASH_ALL)

-- | Construct a batched penalty transaction spending multiple
--   revoked outputs.
--
-- Per BOLT #5, up to 483 bidirectional HTLCs plus to_local can
-- be resolved in a single penalty transaction (within the
-- 400,000 weight limit). The caller signs each input with the
-- revocation privkey.
--
-- Returns 'Nothing' if the total fee would exceed the total
-- input value.
spend_revoked_batch :: PenaltyContext -> Maybe SpendingTx
spend_revoked_batch !ctx =
  let !outs = pc_outputs ctx
      !destScript = pc_destination ctx
      !feerate = pc_feerate ctx

      -- Calculate total input value and weight
      !(totalValue, totalWeight) =
        go (Satoshi 0) penalty_tx_base_weight
          (NE.toList outs)

      !fee = spending_fee feerate totalWeight
  in if unSatoshi fee >= unSatoshi totalValue
     then Nothing
     else
       let !outputValue =
             Satoshi
               (unSatoshi totalValue - unSatoshi fee)

           -- Build inputs
           !txInputs = fmap mkPenaltyInput outs

           -- Single output
           !txOutput = TxOut
             (unSatoshi outputValue)
             (unScript destScript)

           !tx = Tx
             { tx_version   = 2
             , tx_inputs    = txInputs
             , tx_outputs   = txOutput :| []
             , tx_witnesses = []
             , tx_locktime  = 0
             }

           !witnessScript = Script BS.empty
       in Just (SpendingTx tx witnessScript totalValue
                  SIGHASH_ALL)
  where
    go !totalVal !totalWt [] = (totalVal, totalWt)
    go !totalVal !totalWt (ro:rest) =
      let !w = revoked_output_weight ro
          !v = Satoshi
            (unSatoshi totalVal
             + unSatoshi (ro_value ro))
      in go v (totalWt + w) rest

    mkPenaltyInput !ro =
      TxIn
        { txin_prevout = ro_outpoint ro
        , txin_script_sig = BS.empty
        , txin_sequence = 0xFFFFFFFF
        }

-- anchor spends ------------------------------------------------------

-- | Spend an anchor output as the owner (immediately).
--
-- The caller signs with the funding privkey and uses
-- 'anchor_witness_owner' from bolt3.
spend_anchor_owner
  :: OutPoint
  -- ^ Outpoint of the anchor output.
  -> Satoshi
  -- ^ Value of the anchor output (330 sats).
  -> FundingPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> SpendingTx
spend_anchor_owner !op !value !fundpk !destScript =
  let !witnessScript = anchor_script fundpk
      !tx = mk_spending_tx op 0xFFFFFFFE destScript
              value 0
  in SpendingTx tx witnessScript value SIGHASH_ALL

-- | Spend an anchor output as anyone (after 16 blocks).
--
-- Uses 'anchor_witness_anyone' from bolt3 (empty signature).
spend_anchor_anyone
  :: OutPoint
  -- ^ Outpoint of the anchor output.
  -> Satoshi
  -- ^ Value of the anchor output (330 sats).
  -> FundingPubkey
  -> Script
  -- ^ Destination scriptPubKey.
  -> SpendingTx
spend_anchor_anyone !op !value !fundpk !destScript =
  let !witnessScript = anchor_script fundpk
      !tx = mk_spending_tx op 16 destScript value 0
  in SpendingTx tx witnessScript value SIGHASH_ALL

-- internal helpers ---------------------------------------------------

-- | Build a simple single-input single-output spending tx.
mk_spending_tx
  :: OutPoint     -- ^ Input outpoint
  -> Word32       -- ^ Input nSequence
  -> Script       -- ^ Output scriptPubKey
  -> Satoshi      -- ^ Output value
  -> Word32       -- ^ Locktime
  -> Tx
mk_spending_tx !op !seqNo !destScript !outputValue
    !locktime =
  let !txIn = TxIn
        { txin_prevout = op
        , txin_script_sig = BS.empty
        , txin_sequence = seqNo
        }
      !txOut = TxOut
        { txout_value = unSatoshi outputValue
        , txout_script_pubkey = unScript destScript
        }
  in Tx
       { tx_version   = 2
       , tx_inputs    = txIn :| []
       , tx_outputs   = txOut :| []
       , tx_witnesses = []
       , tx_locktime  = locktime
       }

-- | Convert a bolt3 HTLCTx to a ppad-tx Tx.
htlc_tx_to_tx :: HTLCTx -> Tx
htlc_tx_to_tx !htx =
  let !txIn = TxIn
        { txin_prevout = htx_input_outpoint htx
        , txin_script_sig = BS.empty
        , txin_sequence =
            unSequence (htx_input_sequence htx)
        }
      !txOut = TxOut
        { txout_value =
            unSatoshi (htx_output_value htx)
        , txout_script_pubkey =
            unScript (htx_output_script htx)
        }
  in Tx
       { tx_version = htx_version htx
       , tx_inputs = txIn :| []
       , tx_outputs = txOut :| []
       , tx_witnesses = []
       , tx_locktime =
           unLocktime (htx_locktime htx)
       }
