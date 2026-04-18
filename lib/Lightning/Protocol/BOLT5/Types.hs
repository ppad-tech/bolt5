{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT5.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Types for BOLT #5 on-chain transaction handling.

module Lightning.Protocol.BOLT5.Types (
    -- * Close identification
    CloseType(..)

    -- * Output classification
  , UnresolvedOutput(..)
  , OutputResolution(..)

    -- * Spending transactions
  , SpendingTx(..)

    -- * Penalty batching
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
  ) where

import Bitcoin.Prim.Tx (Tx(..))
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT3.Types
import Lightning.Protocol.BOLT3.Tx (
    CommitmentKeys(..)
  , OutputType(..)
  )

-- close identification -----------------------------------------------

-- | What kind of close was detected on chain.
data CloseType
  = MutualClose
    -- ^ Cooperative closure agreed by both parties.
  | LocalCommitClose
    -- ^ Our commitment transaction was broadcast.
  | RemoteCommitClose
    -- ^ The remote party's commitment transaction was broadcast.
  | RevokedCommitClose
    -- ^ A revoked (outdated) commitment transaction was broadcast.
  deriving (Eq, Show, Generic)

-- output classification ----------------------------------------------

-- | An unresolved commitment transaction output.
data UnresolvedOutput = UnresolvedOutput
  { uo_outpoint :: !OutPoint
  , uo_value    :: {-# UNPACK #-} !Satoshi
  , uo_type     :: !OutputResolution
  } deriving (Eq, Show, Generic)

-- | How to resolve an output, per BOLT #5 rules.
data OutputResolution
  = Resolved
    -- ^ Already resolved (e.g. to_remote on local commit).
  | SpendToLocal
      !ToSelfDelay !RevocationPubkey !LocalDelayedPubkey
    -- ^ Spend to_local after CSV delay.
  | SpendHTLCTimeout
      !HTLC !CommitmentKeys !ChannelFeatures
    -- ^ Spend via HTLC-timeout second-stage tx (local commit,
    --   local offer).
  | SpendHTLCSuccess
      !HTLC !CommitmentKeys !ChannelFeatures
    -- ^ Spend via HTLC-success second-stage tx (local commit,
    --   remote offer).
  | SpendHTLCTimeoutDirect !HTLC
    -- ^ Spend HTLC directly after timeout (remote commit,
    --   local offer).
  | SpendHTLCPreimageDirect !HTLC
    -- ^ Spend HTLC directly with preimage (remote commit,
    --   remote offer).
  | SpendSecondStage
      !ToSelfDelay !RevocationPubkey !LocalDelayedPubkey
    -- ^ Spend second-stage HTLC output after CSV delay.
  | Revoke !RevocationPubkey
    -- ^ Spend revoked to_local with revocation key.
  | RevokeHTLC !RevocationPubkey !OutputType
    -- ^ Spend revoked HTLC output with revocation key.
  | AnchorSpend !FundingPubkey
    -- ^ Spend anchor output.
  deriving (Eq, Show, Generic)

-- spending transactions ----------------------------------------------

-- | Unsigned spending transaction, ready for caller to sign.
--
-- The caller uses bolt3 witness constructors to assemble the
-- final witness after signing.
data SpendingTx = SpendingTx
  { stx_tx           :: !Tx
    -- ^ The unsigned transaction.
  , stx_input_script :: !Script
    -- ^ Witness script for the input being spent.
  , stx_input_value  :: {-# UNPACK #-} !Satoshi
    -- ^ Value of the input being spent (for sighash).
  , stx_sighash_type :: !SighashType
    -- ^ Sighash type to use when signing.
  } deriving (Eq, Show, Generic)

-- penalty batching ---------------------------------------------------

-- | Context for constructing batched penalty transactions.
data PenaltyContext = PenaltyContext
  { pc_outputs        :: ![UnresolvedOutput]
    -- ^ Revoked outputs to sweep.
  , pc_revocation_key :: !RevocationPubkey
    -- ^ Revocation pubkey for all outputs.
  , pc_destination    :: !Script
    -- ^ Destination scriptPubKey.
  , pc_feerate        :: !FeeratePerKw
    -- ^ Fee rate for the penalty transaction.
  } deriving (Eq, Show, Generic)

-- weight constants (BOLT #5 Appendix A) ------------------------------

-- | Expected weight of the to_local penalty transaction witness
--   (160 bytes).
to_local_penalty_witness_weight :: Word64
to_local_penalty_witness_weight = 160

-- | Expected weight of the offered_htlc penalty transaction
--   witness (243 bytes).
offered_htlc_penalty_witness_weight :: Word64
offered_htlc_penalty_witness_weight = 243

-- | Expected weight of the accepted_htlc penalty transaction
--   witness (249 bytes).
accepted_htlc_penalty_witness_weight :: Word64
accepted_htlc_penalty_witness_weight = 249

-- | Weight of a to_local penalty input (164 + 160 = 324 bytes).
to_local_penalty_input_weight :: Word64
to_local_penalty_input_weight = 324

-- | Weight of an offered_htlc penalty input
--   (164 + 243 = 407 bytes).
offered_htlc_penalty_input_weight :: Word64
offered_htlc_penalty_input_weight = 407

-- | Weight of an accepted_htlc penalty input
--   (164 + 249 = 413 bytes).
accepted_htlc_penalty_input_weight :: Word64
accepted_htlc_penalty_input_weight = 413

-- | Weight of a to_remote P2WPKH input
--   (108 + 164 = 272 bytes).
to_remote_input_weight :: Word64
to_remote_input_weight = 272

-- | Base weight of a penalty transaction (4*53 + 2 = 214 bytes).
--
-- Non-witness: version(4) + input_count(1) + output_count(1) +
-- value(8) + script_len(1) + p2wsh_script(34) + locktime(4) = 53
-- Witness header: 2 bytes.
penalty_tx_base_weight :: Word64
penalty_tx_base_weight = 214

-- | Maximum standard transaction weight (400,000 bytes).
max_standard_weight :: Word64
max_standard_weight = 400000

-- fee calculation ----------------------------------------------------

-- | Calculate the fee for a spending transaction given its weight.
--
-- @fee = feerate_per_kw * weight / 1000@
spending_fee :: FeeratePerKw -> Word64 -> Satoshi
spending_fee (FeeratePerKw !rate) !weight =
  Satoshi ((fromIntegral rate * weight) `div` 1000)
{-# INLINE spending_fee #-}
