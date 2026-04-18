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
-- This module implements the logic for handling channel closures:
--
-- * Mutual close - cooperative closure agreed by both parties
-- * Unilateral close - one party publishes their commitment
--   transaction
-- * Revoked transaction close - penalty for publishing old state
--
-- = Design
--
-- This is a stateless toolkit of pure functions. The caller
-- manages channel state (which outputs are resolved, current
-- block height, etc.) and provides explicit inputs. Functions
-- produce unsigned 'SpendingTx' values; the caller signs and
-- assembles witnesses using bolt3 constructors.
--
-- = Usage
--
-- @
-- import Lightning.Protocol.BOLT3
-- import Lightning.Protocol.BOLT5
--
-- -- Classify outputs of our local commitment
-- let outputs = classify_local_commit_outputs
--       commitTx keys delay features htlcs
--
-- -- For each unresolved output, construct spending tx
-- case uo_type output of
--   SpendToLocal delay revpk delayedpk ->
--     spend_to_local (uo_outpoint output)
--       (uo_value output) revpk delay delayedpk
--       destScript feerate
--   ...
-- @

module Lightning.Protocol.BOLT5 (
    -- * Types
    -- ** Close identification
    CloseType(..)

    -- ** Output classification
  , UnresolvedOutput(..)
  , OutputResolution(..)

    -- ** Spending transactions
  , SpendingTx(..)

    -- ** Penalty batching
  , PenaltyContext(..)

    -- * Weight constants (Appendix A)
  , to_local_penalty_witness_weight
  , offered_htlc_penalty_witness_weight
  , accepted_htlc_penalty_witness_weight
  , to_local_penalty_input_weight
  , offered_htlc_penalty_input_weight
  , accepted_htlc_penalty_input_weight
  , to_remote_input_weight
  , penalty_tx_base_weight
  , max_standard_weight

    -- * Fee calculation
  , spending_fee

    -- * Close identification
  , identify_close

    -- * Output classification
  , classify_local_commit_outputs
  , classify_remote_commit_outputs
  , classify_revoked_commit_outputs

    -- * Preimage extraction
  , extract_preimage_offered
  , extract_preimage_htlc_success

    -- * Timeout check
  , htlc_timed_out

    -- * Spending transaction construction
    -- ** Local commitment
  , spend_to_local
  , spend_htlc_timeout
  , spend_htlc_success
  , spend_htlc_output

    -- ** Remote commitment
  , spend_remote_htlc_timeout
  , spend_remote_htlc_preimage

    -- ** Revoked commitment
  , spend_revoked_to_local
  , spend_revoked_htlc
  , spend_revoked_htlc_output
  , spend_revoked_batch

    -- ** Anchor outputs
  , spend_anchor_owner
  , spend_anchor_anyone
  ) where

import Lightning.Protocol.BOLT5.Types
import Lightning.Protocol.BOLT5.Detect
import Lightning.Protocol.BOLT5.Spend
