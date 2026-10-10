{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT5
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- On-chain transaction handling for the Lightning Network, per
-- [BOLT #5](https://github.com/lightning/bolts/blob/master/05-onchain.md).
--
-- This library's scope is being redesigned, and it is not yet ready
-- for release. It covers unilateral closes only partially: it
-- identifies a local or remote commitment on chain, classifies its
-- outputs, extracts preimages from HTLC spends, and builds unsigned,
-- single-input spending transactions. It doesn't detect mutual or
-- revoked closes, assemble witnesses, or attach fee inputs to anchor
-- spends and zero-fee HTLC transactions.
--
-- = Design
--
-- This is a stateless toolkit of pure functions. The caller manages
-- channel state (which outputs are resolved, the current block
-- height, etc.) and provides explicit inputs. Commitment
-- transactions are those built by ppad-bolt3, and amounts are
-- ppad-bolt1's. Functions produce unsigned 'SpendingTx' values; the
-- caller signs them and assembles witnesses with bolt3's witness
-- functions.
--
-- = Usage
--
-- @
-- import qualified Lightning.Protocol.BOLT1 as BOLT1
-- import Lightning.Protocol.BOLT3
-- import Lightning.Protocol.BOLT5
--
-- -- classify the outputs of our local commitment
-- let outputs = classify_local_commit_outputs commit keys delay fmt
--
-- -- for each unresolved output, build a spending transaction
-- case uo_type output of
--   SpendToLocal delay revpk delayedpk ->
--     spend_to_local (uo_outpoint output) (uo_value output)
--       revpk delay delayedpk dest feerate
--   ...
-- @

module Lightning.Protocol.BOLT5 (
    -- * Close identification
    CloseType(..)
  , identify_close

    -- * Output classification
  , UnresolvedOutput(..)
  , OutputResolution(..)
  , classify_local_commit_outputs
  , classify_remote_commit_outputs
  , classify_revoked_commit_outputs

    -- * Preimage extraction
  , extract_preimage_offered
  , extract_preimage_htlc_success

    -- * Timeout check
  , htlc_timed_out

    -- * Spending transactions
  , SpendingTx(..)

    -- ** Local commitment
  , spend_to_local
  , spend_htlc_tx
  , spend_htlc_output

    -- ** Remote commitment
  , spend_remote_htlc_timeout
  , spend_remote_htlc_preimage

    -- ** Revoked commitment
  , spend_revoked_to_local
  , spend_revoked_htlc
  , spend_revoked_htlc_output

    -- ** Anchor outputs
  , spend_anchor_owner
  , spend_anchor_anyone

    -- * Revoked outputs
  , HTLCOutputType(..)
  , htlc_output_type
  , RevokedOutput(..)
  , RevokedOutputType(..)

    -- * Weights (Appendix A)
  , revoked_output_weight
  , htlc_output_type_weight
  , to_local_penalty_witness_weight
  , offered_htlc_penalty_witness_weight
  , accepted_htlc_penalty_witness_weight
  , offered_htlc_penalty_witness_weight_anchors
  , accepted_htlc_penalty_witness_weight_anchors
  , to_local_penalty_input_weight
  , offered_htlc_penalty_input_weight
  , accepted_htlc_penalty_input_weight
  , offered_htlc_penalty_input_weight_anchors
  , accepted_htlc_penalty_input_weight_anchors
  , to_remote_input_weight
  , penalty_tx_base_weight
  , max_standard_weight

    -- * Fees
  , spending_fee
  ) where

import Lightning.Protocol.BOLT5.Detect
import Lightning.Protocol.BOLT5.Spend
import Lightning.Protocol.BOLT5.Types
