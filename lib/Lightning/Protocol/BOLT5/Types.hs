{-# OPTIONS_HADDOCK hide #-}
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

    -- * HTLC output type
  , HTLCOutputType(..)
  , htlc_output_type
  , htlc_output_type_weight

    -- * Spending transactions
  , SpendingTx(..)

    -- * Revoked outputs
  , RevokedOutput(..)
  , RevokedOutputType(..)
  , revoked_output_weight

    -- * Weight constants (Appendix A)
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

    -- * Fee calculation
  , spending_fee
  ) where

import Bitcoin.Prim.Tx (OutPoint, Tx)
import Bitcoin.Prim.Tx.Sighash (SighashType)
import Control.DeepSeq (NFData(..))
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (Satoshi)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3

-- close identification -----------------------------------------------

-- | What kind of close was detected on chain.
--
--   @identify_close@ produces only 'LocalCommitClose' and
--   'RemoteCommitClose'; recognising mutual and revoked closes is
--   left to the caller.
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

instance NFData CloseType

-- output classification ----------------------------------------------

-- | An unresolved commitment transaction output.
data UnresolvedOutput = UnresolvedOutput
  { uo_outpoint :: !OutPoint
  , uo_value    :: {-# UNPACK #-} !Satoshi
  , uo_type     :: !OutputResolution
  } deriving (Eq, Show, Generic)

instance NFData UnresolvedOutput

-- | How to resolve an output, per BOLT #5 rules.
data OutputResolution
  = Resolved
    -- ^ Nothing to do (e.g. to_remote on our local commitment).
  | SpendToLocal
      !ToSelfDelay !RevocationPubkey !LocalDelayedPubkey
    -- ^ Spend to_local after the CSV delay.
  | SpendHTLCTimeout
      !HTLC !CommitmentKeys !CommitmentFormat
    -- ^ Spend via an HTLC-timeout transaction (our commitment, an
    --   HTLC we offered).
  | SpendHTLCSuccess
      !HTLC !CommitmentKeys !CommitmentFormat
    -- ^ Spend via an HTLC-success transaction (our commitment, an
    --   HTLC we received).
  | SpendHTLCTimeoutDirect !HTLC
    -- ^ Spend the HTLC directly after it times out (the remote
    --   commitment, an HTLC we offered).
  | SpendHTLCPreimageDirect !HTLC
    -- ^ Spend the HTLC directly with the preimage (the remote
    --   commitment, an HTLC we received).
  | Revoke !RevocationPubkey
    -- ^ Spend a revoked to_local output with the revocation key.
  | RevokeHTLC !RevocationPubkey !HTLCOutputType
    -- ^ Spend a revoked HTLC output with the revocation key.
  | AnchorSpend !FundingPubkey
    -- ^ Spend our anchor output.
  deriving (Eq, Show, Generic)

instance NFData OutputResolution

-- HTLC output type -------------------------------------------------

-- | The kind of an HTLC output, from the commitment owner's point of
--   view.
--
--   Unlike bolt3's 'OutputType', this can't describe a to_local,
--   to_remote or anchor output.
data HTLCOutputType
  = HTLCOfferedOutput {-# UNPACK #-} !CltvExpiry
    -- ^ Offered HTLC output with CLTV expiry.
  | HTLCReceivedOutput {-# UNPACK #-} !CltvExpiry
    -- ^ Received HTLC output with CLTV expiry.
  deriving (Eq, Show, Generic)

instance NFData HTLCOutputType

-- | The 'HTLCOutputType' of a bolt3 'OutputType', or 'Nothing' if
--   it isn't an HTLC output.
--
--   >>> htlc_output_type OutputToLocal
--   Nothing
htlc_output_type :: OutputType -> Maybe HTLCOutputType
htlc_output_type t = case t of
  OutputHTLC h -> Just $ case htlc_direction h of
    HTLCOffered  -> HTLCOfferedOutput (htlc_cltv_expiry h)
    HTLCReceived -> HTLCReceivedOutput (htlc_cltv_expiry h)
  OutputToLocal      -> Nothing
  OutputToRemote     -> Nothing
  OutputLocalAnchor  -> Nothing
  OutputRemoteAnchor -> Nothing
{-# INLINE htlc_output_type #-}

-- | Penalty input weight for an HTLC output.
--
--   >>> htlc_output_type_weight Anchors (HTLCOfferedOutput (CltvExpiry 500))
--   410
htlc_output_type_weight :: CommitmentFormat -> HTLCOutputType -> Word64
htlc_output_type_weight !fmt !t = case (fmt, t) of
  (StaticRemotekey, HTLCOfferedOutput _)  ->
    offered_htlc_penalty_input_weight
  (StaticRemotekey, HTLCReceivedOutput _) ->
    accepted_htlc_penalty_input_weight
  (Anchors, HTLCOfferedOutput _)          ->
    offered_htlc_penalty_input_weight_anchors
  (Anchors, HTLCReceivedOutput _)         ->
    accepted_htlc_penalty_input_weight_anchors
{-# INLINE htlc_output_type_weight #-}

-- spending transactions ----------------------------------------------

-- | An unsigned spending transaction, with what the caller needs to
--   sign its (single) input.
--
--   The caller signs the input's BIP143 sighash (ppad-tx's
--   @sighash_segwit@ over 'stx_tx', input 0, 'stx_input_script',
--   'stx_input_value' and 'stx_sighash_type') and assembles the
--   witness with bolt3's witness functions.
data SpendingTx = SpendingTx
  { stx_tx           :: !Tx
    -- ^ The unsigned transaction.
  , stx_input_script :: !Script
    -- ^ Witness script of the output being spent.
  , stx_input_value  :: {-# UNPACK #-} !Satoshi
    -- ^ Value of the output being spent.
  , stx_sighash_type :: !SighashType
    -- ^ Sighash type of the local signature.
  } deriving (Eq, Show, Generic)

instance NFData SpendingTx where
  rnf (SpendingTx tx s v h) = rnf tx `seq` rnf s `seq` rnf v `seq` h `seq` ()

-- revoked outputs ----------------------------------------------------

-- | A revoked output that can be swept with a revocation key.
data RevokedOutput = RevokedOutput
  { ro_outpoint :: !OutPoint
  , ro_value    :: {-# UNPACK #-} !Satoshi
  , ro_type     :: !RevokedOutputType
  } deriving (Eq, Show, Generic)

instance NFData RevokedOutput

-- | What kind of revoked output is being swept.
data RevokedOutputType
  = RevokedToLocal
    -- ^ Revoked to_local output.
  | RevokedHTLC !HTLCOutputType
    -- ^ Revoked HTLC output.
  deriving (Eq, Show, Generic)

instance NFData RevokedOutputType

-- | Penalty input weight for a revoked output.
revoked_output_weight :: CommitmentFormat -> RevokedOutput -> Word64
revoked_output_weight !fmt !ro = case ro_type ro of
  RevokedToLocal ->
    to_local_penalty_input_weight
  RevokedHTLC !htype ->
    htlc_output_type_weight fmt htype
{-# INLINE revoked_output_weight #-}

-- weight constants (BOLT #5 Appendix A) ------------------------------

-- | Expected weight of the to_local penalty transaction witness
--   (160 weight units).
to_local_penalty_witness_weight :: Word64
to_local_penalty_witness_weight = 160

-- | Expected weight of the offered_htlc penalty transaction
--   witness (243 weight units), for a 133-byte witness script
--   (no option_anchors).
offered_htlc_penalty_witness_weight :: Word64
offered_htlc_penalty_witness_weight = 243

-- | Expected weight of the accepted_htlc penalty transaction
--   witness (249 weight units), for a 139-byte witness script
--   (no option_anchors).
accepted_htlc_penalty_witness_weight :: Word64
accepted_htlc_penalty_witness_weight = 249

-- | Expected weight of the offered_htlc penalty transaction
--   witness with option_anchors (246 weight units), whose witness
--   script is 136 bytes.
offered_htlc_penalty_witness_weight_anchors :: Word64
offered_htlc_penalty_witness_weight_anchors = 246

-- | Expected weight of the accepted_htlc penalty transaction
--   witness with option_anchors (252 weight units), whose witness
--   script is 142 bytes.
accepted_htlc_penalty_witness_weight_anchors :: Word64
accepted_htlc_penalty_witness_weight_anchors = 252

-- | Weight of a to_local penalty input (164 + 160 = 324).
to_local_penalty_input_weight :: Word64
to_local_penalty_input_weight = 324

-- | Weight of an offered_htlc penalty input (164 + 243 = 407).
offered_htlc_penalty_input_weight :: Word64
offered_htlc_penalty_input_weight = 407

-- | Weight of an accepted_htlc penalty input (164 + 249 = 413).
accepted_htlc_penalty_input_weight :: Word64
accepted_htlc_penalty_input_weight = 413

-- | Weight of an offered_htlc penalty input with option_anchors
--   (164 + 246 = 410).
offered_htlc_penalty_input_weight_anchors :: Word64
offered_htlc_penalty_input_weight_anchors = 410

-- | Weight of an accepted_htlc penalty input with option_anchors
--   (164 + 252 = 416).
accepted_htlc_penalty_input_weight_anchors :: Word64
accepted_htlc_penalty_input_weight_anchors = 416

-- | Weight of a to_remote P2WPKH input (108 + 164 = 272).
to_remote_input_weight :: Word64
to_remote_input_weight = 272

-- | Base weight of a penalty transaction (4 * 53 + 2 = 214).
--
--   Non-witness: version (4) + input count (1) + output count (1) +
--   value (8) + script length (1) + P2WSH script (34) + locktime (4)
--   = 53 bytes. Witness header: 2 bytes.
--
--   The spending transactions here use it as their base weight
--   whatever their destination script.
penalty_tx_base_weight :: Word64
penalty_tx_base_weight = 214

-- | Maximum standard transaction weight (400,000).
max_standard_weight :: Word64
max_standard_weight = 400000

-- fee calculation ----------------------------------------------------

-- | The fee for a transaction of the given weight:
--
--   @fee = feerate_per_kw * weight / 1000@
--
--   rounded down, and saturating at 21 million BTC.
--
--   >>> spending_fee (FeeratePerKw 253) 324
--   Satoshi 81
spending_fee :: FeeratePerKw -> Word64 -> Satoshi
spending_fee (FeeratePerKw !rate) !weight =
  let !f = toInteger rate * toInteger weight `quot` 1000
      !m = BOLT1.un_satoshi BOLT1.max_satoshi
  in  case BOLT1.satoshi (fromInteger (min f (toInteger m))) of
        Just s  -> s
        Nothing -> BOLT1.max_satoshi
{-# INLINE spending_fee #-}
