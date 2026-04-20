{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Lightning.Protocol.BOLT3 hiding
  (txout_value, txout_script)
import qualified Lightning.Protocol.BOLT5 as B5
import Weigh

-- NFData orphan instances for weigh ---------------------------------

instance NFData Satoshi where
  rnf (Satoshi x) = rnf x

instance NFData Script where
  rnf (Script bs) = rnf bs

instance NFData Pubkey where
  rnf (Pubkey x) = rnf x

instance NFData Point where
  rnf (Point x) = rnf x

instance NFData RevocationPubkey where
  rnf (RevocationPubkey x) = rnf x

instance NFData LocalDelayedPubkey where
  rnf (LocalDelayedPubkey p) = rnf p

instance NFData FundingPubkey where
  rnf (FundingPubkey p) = rnf p

instance NFData FeeratePerKw where
  rnf (FeeratePerKw x) = rnf x

instance NFData ToSelfDelay where
  rnf (ToSelfDelay x) = rnf x

instance NFData CltvExpiry where
  rnf (CltvExpiry x) = rnf x

instance NFData SighashType

instance NFData B5.OutputResolution where
  rnf B5.Resolved = ()
  rnf (B5.Revoke rk) = rnf rk
  rnf _ = ()

instance NFData B5.HTLCOutputType where
  rnf (B5.HTLCOfferedOutput e) = rnf e
  rnf (B5.HTLCReceivedOutput e) = rnf e

instance NFData B5.UnresolvedOutput where
  rnf (B5.UnresolvedOutput op v t) =
    rnf op `seq` rnf v `seq` rnf t

instance NFData B5.RevokedOutputType where
  rnf B5.RevokedToLocal = ()
  rnf (B5.RevokedHTLC h) = rnf h

instance NFData B5.RevokedOutput where
  rnf (B5.RevokedOutput op v t) =
    rnf op `seq` rnf v `seq` rnf t

instance NFData B5.SpendingTx where
  rnf (B5.SpendingTx tx scr val sh) =
    rnf tx `seq` rnf scr `seq` rnf val `seq` rnf sh

instance NFData B5.PenaltyContext where
  rnf (B5.PenaltyContext os rk d f) =
    rnf os `seq` rnf rk `seq` rnf d `seq` rnf f

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith $ do
  spend_weights
  batch_weights

-- fixtures -----------------------------------------------------------

dummyPubkey :: Pubkey
dummyPubkey = case pubkey (BS.pack (0x02 : replicate 32 0x01)) of
  Just pk -> pk
  Nothing -> error "impossible"

dummyTxId :: TxId
dummyTxId = case mkTxId (BS.replicate 32 0x00) of
  Just tid -> tid
  Nothing  -> error "impossible"

dummyOutPoint :: OutPoint
dummyOutPoint = OutPoint dummyTxId 0

dummyRevPk :: RevocationPubkey
dummyRevPk = RevocationPubkey dummyPubkey

dummyDelayedPk :: LocalDelayedPubkey
dummyDelayedPk = LocalDelayedPubkey dummyPubkey

dummyFundPk :: FundingPubkey
dummyFundPk = FundingPubkey dummyPubkey

dummyDest :: Script
dummyDest = Script $ BS.pack [0x00, 0x14] <>
  BS.replicate 20 0xCC

dummyFeerate :: FeeratePerKw
dummyFeerate = FeeratePerKw 253

dummyDelay :: ToSelfDelay
dummyDelay = ToSelfDelay 144

mkRevokedOutputs :: Int -> NE.NonEmpty B5.RevokedOutput
mkRevokedOutputs n =
  let ro i = B5.RevokedOutput
        (OutPoint dummyTxId (fromIntegral i))
        (Satoshi 10000)
        B5.RevokedToLocal
  in  ro 0 NE.:| [ ro i | i <- [1..n-1] ]

-- weights ------------------------------------------------------------

spend_weights :: Weigh ()
spend_weights = wgroup "spend" $ do
  func "spend_to_local"
    (\v -> B5.spend_to_local
      dummyOutPoint v dummyRevPk dummyDelay
      dummyDelayedPk dummyDest dummyFeerate)
    (Satoshi 100000)

  func "spend_anchor_owner"
    (\v -> B5.spend_anchor_owner
      dummyOutPoint v dummyFundPk dummyDest)
    (Satoshi 330)

batch_weights :: Weigh ()
batch_weights = wgroup "batch" $ do
  func "spend_revoked_batch/10"
    B5.spend_revoked_batch
    (B5.PenaltyContext
      (mkRevokedOutputs 10)
      dummyRevPk dummyDest dummyFeerate)

  func "spend_revoked_batch/100"
    B5.spend_revoked_batch
    (B5.PenaltyContext
      (mkRevokedOutputs 100)
      dummyRevPk dummyDest dummyFeerate)

  func "spend_revoked_batch/483"
    B5.spend_revoked_batch
    (B5.PenaltyContext
      (mkRevokedOutputs 483)
      dummyRevPk dummyDest dummyFeerate)
