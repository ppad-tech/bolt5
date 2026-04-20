{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Lightning.Protocol.BOLT3 hiding
  (txout_value, txout_script)
import qualified Lightning.Protocol.BOLT5 as B5

-- NFData orphan instances for criterion -----------------------------

instance NFData Satoshi where
  rnf (Satoshi x) = rnf x

instance NFData MilliSatoshi where
  rnf (MilliSatoshi x) = rnf x

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

instance NFData PaymentHash where
  rnf (PaymentHash x) = rnf x

instance NFData CltvExpiry where
  rnf (CltvExpiry x) = rnf x

instance NFData HTLCDirection where
  rnf HTLCOffered = ()
  rnf HTLCReceived = ()

instance NFData HTLC where
  rnf (HTLC d a h c) =
    rnf d `seq` rnf a `seq` rnf h `seq` rnf c

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

main :: IO ()
main = defaultMain [
    spend_benchmarks
  , classify_benchmarks
  ]

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

-- benchmarks ---------------------------------------------------------

spend_benchmarks :: Benchmark
spend_benchmarks = bgroup "spend" [
    bench "spend_to_local" $
      nf (\v -> B5.spend_to_local
            dummyOutPoint v dummyRevPk dummyDelay
            dummyDelayedPk dummyDest dummyFeerate)
        (Satoshi 100000)

  , bench "spend_revoked_to_local" $
      nf (\v -> B5.spend_revoked_to_local
            dummyOutPoint v dummyRevPk dummyDelay
            dummyDelayedPk dummyDest dummyFeerate)
        (Satoshi 100000)

  , bench "spend_anchor_owner" $
      nf (\v -> B5.spend_anchor_owner
            dummyOutPoint v dummyFundPk dummyDest)
        (Satoshi 330)

  , bench "spend_revoked_batch/10" $
      nf B5.spend_revoked_batch
        (B5.PenaltyContext
          (mkRevokedOutputs 10)
          dummyRevPk dummyDest dummyFeerate)

  , bench "spend_revoked_batch/100" $
      nf B5.spend_revoked_batch
        (B5.PenaltyContext
          (mkRevokedOutputs 100)
          dummyRevPk dummyDest dummyFeerate)

  , bench "spend_revoked_batch/483" $
      nf B5.spend_revoked_batch
        (B5.PenaltyContext
          (mkRevokedOutputs 483)
          dummyRevPk dummyDest dummyFeerate)
  ]

classify_benchmarks :: Benchmark
classify_benchmarks = bgroup "classify" [
    bench "spending_fee" $
      nf (B5.spending_fee dummyFeerate) 538

  , bench "htlc_timed_out" $
      nf (B5.htlc_timed_out 500001)
        (HTLC HTLCOffered (MilliSatoshi 1000000)
              dummyPaymentHash (CltvExpiry 500000))
  ]
  where
    dummyPaymentHash = case paymentHash
      (BS.replicate 32 0xAA) of
        Just ph -> ph
        Nothing -> error "impossible"
