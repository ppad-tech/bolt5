{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitcoin.Prim.Tx (TxOut(..))
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Lightning.Protocol.BOLT3 hiding
  (txout_value, txout_script)
import qualified Lightning.Protocol.BOLT5 as B5
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt5" [
    types_tests
  , detect_tests
  , classify_tests
  , spend_tests
  , htlc_spend_tests
  , remote_spend_tests
  , revoked_htlc_spend_tests
  , weight_tests
  , property_tests
  ]

-- test fixtures ------------------------------------------------------

-- Dummy 33-byte pubkey
dummyPubkeyBytes :: BS.ByteString
dummyPubkeyBytes = BS.pack $
  0x02 : replicate 32 0x01

-- Dummy 32-byte hash
dummyHash32 :: BS.ByteString
dummyHash32 = BS.replicate 32 0xAA

-- Dummy 32-byte preimage
dummyPreimage :: BS.ByteString
dummyPreimage = BS.replicate 32 0xBB

dummyTxId :: TxId
dummyTxId = case mkTxId (BS.replicate 32 0x00) of
  Just tid -> tid
  Nothing  -> error "impossible"

dummyOutPoint :: OutPoint
dummyOutPoint = OutPoint dummyTxId 0

dummyPubkey :: Pubkey
dummyPubkey = case pubkey dummyPubkeyBytes of
  Just pk -> pk
  Nothing -> error "impossible"

dummyRevocationPubkey :: RevocationPubkey
dummyRevocationPubkey = RevocationPubkey dummyPubkey

dummyLocalDelayedPubkey :: LocalDelayedPubkey
dummyLocalDelayedPubkey = LocalDelayedPubkey dummyPubkey

dummyLocalHtlcPubkey :: LocalHtlcPubkey
dummyLocalHtlcPubkey = LocalHtlcPubkey dummyPubkey

dummyRemoteHtlcPubkey :: RemoteHtlcPubkey
dummyRemoteHtlcPubkey = RemoteHtlcPubkey dummyPubkey

dummyFundingPubkey :: FundingPubkey
dummyFundingPubkey = FundingPubkey dummyPubkey

dummyPaymentHash :: PaymentHash
dummyPaymentHash = case payment_hash dummyHash32 of
  Just ph -> ph
  Nothing -> error "impossible"

dummyDelay :: ToSelfDelay
dummyDelay = ToSelfDelay 144

dummyFeerate :: FeeratePerKw
dummyFeerate = FeeratePerKw 253

dummyFeatures :: ChannelFeatures
dummyFeatures = ChannelFeatures False

dummyFeaturesAnchors :: ChannelFeatures
dummyFeaturesAnchors = ChannelFeatures True

dummyDestScript :: Script
dummyDestScript = Script $ BS.pack
  [0x00, 0x14] <> BS.replicate 20 0xCC

dummyKeys :: CommitmentKeys
dummyKeys = CommitmentKeys
  { ck_revocation_pubkey = dummyRevocationPubkey
  , ck_local_delayed = dummyLocalDelayedPubkey
  , ck_local_htlc = dummyLocalHtlcPubkey
  , ck_remote_htlc = dummyRemoteHtlcPubkey
  , ck_local_payment = LocalPubkey dummyPubkey
  , ck_remote_payment = RemotePubkey dummyPubkey
  , ck_local_funding = dummyFundingPubkey
  , ck_remote_funding = dummyFundingPubkey
  }

dummyHTLC :: HTLC
dummyHTLC = HTLC
  { htlc_direction = HTLCOffered
  , htlc_amount_msat = MilliSatoshi 1000000
  , htlc_payment_hash = dummyPaymentHash
  , htlc_cltv_expiry = CltvExpiry 500000
  }

dummyReceivedHTLC :: HTLC
dummyReceivedHTLC = dummyHTLC
  { htlc_direction = HTLCReceived }

dummyHTLCContext :: HTLCContext
dummyHTLCContext = HTLCContext
  { hc_commitment_txid = dummyTxId
  , hc_output_index = 0
  , hc_htlc = dummyHTLC
  , hc_to_self_delay = dummyDelay
  , hc_feerate = dummyFeerate
  , hc_features = dummyFeatures
  , hc_revocation_pubkey = dummyRevocationPubkey
  , hc_local_delayed = dummyLocalDelayedPubkey
  }

-- Script that matches what findHTLC expects for offered
offeredScript :: Script
offeredScript = to_p2wsh $ offered_htlc_script
  dummyRevocationPubkey
  dummyRemoteHtlcPubkey
  dummyLocalHtlcPubkey
  dummyPaymentHash
  dummyFeatures

-- Script that matches what findHTLC expects for received
receivedScript :: Script
receivedScript = to_p2wsh $ received_htlc_script
  dummyRevocationPubkey
  dummyRemoteHtlcPubkey
  dummyLocalHtlcPubkey
  dummyPaymentHash
  (CltvExpiry 500000)
  dummyFeatures

toLocalScript :: Script
toLocalScript = to_p2wsh $ to_local_script
  dummyRevocationPubkey dummyDelay
  dummyLocalDelayedPubkey

-- CommitmentTx with to_local + to_remote outputs
dummyLocalCommitTx :: CommitmentTx
dummyLocalCommitTx = CommitmentTx
  { ctx_version = 2
  , ctx_locktime = Locktime 0
  , ctx_input_outpoint = dummyOutPoint
  , ctx_input_sequence = Sequence 0
  , ctx_outputs =
      [ TxOutput (Satoshi 50000) toLocalScript
          OutputToLocal
      , TxOutput (Satoshi 30000) dummyDestScript
          OutputToRemote
      ]
  , ctx_funding_script = dummyDestScript
  }

-- CommitmentTx with HTLC outputs
dummyLocalCommitWithHTLCs :: CommitmentTx
dummyLocalCommitWithHTLCs = CommitmentTx
  { ctx_version = 2
  , ctx_locktime = Locktime 0
  , ctx_input_outpoint = dummyOutPoint
  , ctx_input_sequence = Sequence 0
  , ctx_outputs =
      [ TxOutput (Satoshi 50000) toLocalScript
          OutputToLocal
      , TxOutput (Satoshi 30000) dummyDestScript
          OutputToRemote
      , TxOutput (Satoshi 10000) offeredScript
          (OutputOfferedHTLC (CltvExpiry 500000))
      , TxOutput (Satoshi 10000) receivedScript
          (OutputReceivedHTLC (CltvExpiry 500000))
      ]
  , ctx_funding_script = dummyDestScript
  }

-- A distinct CommitmentTx (different locktime)
dummyRemoteCommitTx :: CommitmentTx
dummyRemoteCommitTx = CommitmentTx
  { ctx_version = 2
  , ctx_locktime = Locktime 1
  , ctx_input_outpoint = dummyOutPoint
  , ctx_input_sequence = Sequence 0
  , ctx_outputs =
      [ TxOutput (Satoshi 40000) toLocalScript
          OutputToLocal
      , TxOutput (Satoshi 20000) dummyDestScript
          OutputToRemote
      ]
  , ctx_funding_script = dummyDestScript
  }

-- types tests --------------------------------------------------------

types_tests :: TestTree
types_tests = testGroup "Types" [
    testCase "CloseType constructors" $ do
      B5.MutualClose @?= B5.MutualClose
      B5.LocalCommitClose @?= B5.LocalCommitClose
      B5.RemoteCommitClose @?= B5.RemoteCommitClose
      B5.RevokedCommitClose @?= B5.RevokedCommitClose

  , testCase "spending_fee calculation" $ do
      let fee = B5.spending_fee (FeeratePerKw 1000) 500
      fee @?= Satoshi 500

  , testCase "spending_fee at low feerate" $ do
      let fee = B5.spending_fee (FeeratePerKw 253) 324
      -- 253 * 324 / 1000 = 81.972 -> 81
      fee @?= Satoshi 81

  , testCase "weight constants" $ do
      B5.to_local_penalty_witness_weight @?= 160
      B5.to_local_penalty_input_weight @?= 324
      B5.offered_htlc_penalty_input_weight @?= 407
      B5.accepted_htlc_penalty_input_weight @?= 413
      B5.to_remote_input_weight @?= 272
      B5.max_standard_weight @?= 400000
  ]

-- detect tests -------------------------------------------------------

detect_tests :: TestTree
detect_tests = testGroup "Detect" [
    testCase "extract_preimage_offered - valid" $ do
      let sig = BS.replicate 72 0x30
          preimage = dummyPreimage
          wit = Witness [sig, preimage]
      case B5.extract_preimage_offered wit of
        Just (PaymentPreimage bs) ->
          bs @?= preimage
        Nothing ->
          assertFailure "expected preimage"

  , testCase "extract_preimage_offered - wrong length" $ do
      let sig = BS.replicate 72 0x30
          badPreimage = BS.replicate 31 0xBB
          wit = Witness [sig, badPreimage]
      B5.extract_preimage_offered wit @?= Nothing

  , testCase "extract_preimage_offered - wrong count" $ do
      let sig = BS.replicate 72 0x30
          wit = Witness [sig]
      B5.extract_preimage_offered wit @?= Nothing

  , testCase "extract_preimage_htlc_success - valid" $ do
      let zero = BS.empty
          remoteSig = BS.replicate 72 0x30
          localSig = BS.replicate 72 0x30
          preimage = dummyPreimage
          wit = Witness [zero, remoteSig, localSig, preimage]
      case B5.extract_preimage_htlc_success wit of
        Just (PaymentPreimage bs) ->
          bs @?= preimage
        Nothing ->
          assertFailure "expected preimage"

  , testCase "extract_preimage_htlc_success - wrong count" $ do
      let wit = Witness [BS.empty, dummyPreimage]
      B5.extract_preimage_htlc_success wit @?= Nothing

  , testCase "htlc_timed_out - at expiry" $ do
      let htlc = dummyHTLC
            { htlc_cltv_expiry = CltvExpiry 500000 }
      B5.htlc_timed_out 500000 htlc @?= True

  , testCase "htlc_timed_out - past expiry" $ do
      let htlc = dummyHTLC
            { htlc_cltv_expiry = CltvExpiry 500000 }
      B5.htlc_timed_out 500001 htlc @?= True

  , testCase "htlc_timed_out - before expiry" $ do
      let htlc = dummyHTLC
            { htlc_cltv_expiry = CltvExpiry 500000 }
      B5.htlc_timed_out 499999 htlc @?= False
  ]

-- classify tests -----------------------------------------------------

classify_tests :: TestTree
classify_tests = testGroup "Classify" [
    testCase "identify_close - local commit" $ do
      case encode_tx_for_signing dummyLocalCommitTx of
        Nothing -> assertFailure "encode failed"
        Just localBytes ->
          B5.identify_close
            dummyLocalCommitTx
            dummyRemoteCommitTx
            localBytes
          @?= Just B5.LocalCommitClose

  , testCase "identify_close - remote commit" $ do
      case encode_tx_for_signing dummyRemoteCommitTx of
        Nothing -> assertFailure "encode failed"
        Just remoteBytes ->
          B5.identify_close
            dummyLocalCommitTx
            dummyRemoteCommitTx
            remoteBytes
          @?= Just B5.RemoteCommitClose

  , testCase "identify_close - no match" $ do
      B5.identify_close
        dummyLocalCommitTx
        dummyRemoteCommitTx
        "unknown bytes"
      @?= Nothing

  , testCase "classify_local - to_local and to_remote" $ do
      let outs = B5.classify_local_commit_outputs
            dummyLocalCommitTx dummyKeys
            dummyDelay dummyFeatures []
      length outs @?= 2
      case outs of
        [o1, o2] -> do
          case B5.uo_type o1 of
            B5.SpendToLocal d rk dk -> do
              d @?= dummyDelay
              rk @?= dummyRevocationPubkey
              dk @?= dummyLocalDelayedPubkey
            other -> assertFailure $
              "expected SpendToLocal, got " <> show other
          B5.uo_type o2 @?= B5.Resolved
        _ -> assertFailure "expected 2 outputs"

  , testCase "classify_local - HTLC outputs" $ do
      let outs = B5.classify_local_commit_outputs
            dummyLocalCommitWithHTLCs dummyKeys
            dummyDelay dummyFeatures
            [dummyHTLC, dummyReceivedHTLC]
      length outs @?= 4
      case outs of
        [_, _, o3, o4] -> do
          case B5.uo_type o3 of
            B5.SpendHTLCTimeout _ _ _ -> pure ()
            other -> assertFailure $
              "expected SpendHTLCTimeout, got "
              <> show other
          case B5.uo_type o4 of
            B5.SpendHTLCSuccess _ _ _ -> pure ()
            other -> assertFailure $
              "expected SpendHTLCSuccess, got "
              <> show other
        _ -> assertFailure "expected 4 outputs"

  , testCase "classify_remote - HTLC outputs" $ do
      let commitTx = CommitmentTx
            { ctx_version = 2
            , ctx_locktime = Locktime 0
            , ctx_input_outpoint = dummyOutPoint
            , ctx_input_sequence = Sequence 0
            , ctx_outputs =
                [ TxOutput (Satoshi 50000) toLocalScript
                    OutputToLocal
                , TxOutput (Satoshi 10000) offeredScript
                    (OutputOfferedHTLC
                      (CltvExpiry 500000))
                , TxOutput (Satoshi 10000) receivedScript
                    (OutputReceivedHTLC
                      (CltvExpiry 500000))
                ]
            , ctx_funding_script = dummyDestScript
            }
          outs = B5.classify_remote_commit_outputs
            commitTx dummyKeys dummyFeatures
            [dummyHTLC, dummyReceivedHTLC]
      length outs @?= 3
      case outs of
        [o1, o2, o3] -> do
          B5.uo_type o1 @?= B5.Resolved
          case B5.uo_type o2 of
            B5.SpendHTLCPreimageDirect _ -> pure ()
            other -> assertFailure $
              "expected SpendHTLCPreimageDirect, got "
              <> show other
          case B5.uo_type o3 of
            B5.SpendHTLCTimeoutDirect _ -> pure ()
            other -> assertFailure $
              "expected SpendHTLCTimeoutDirect, got "
              <> show other
        _ -> assertFailure "expected 3 outputs"

  , testCase "classify_revoked - to_local revoked" $ do
      let outs = B5.classify_revoked_commit_outputs
            dummyLocalCommitWithHTLCs dummyKeys
            dummyRevocationPubkey dummyFeatures
            [dummyHTLC, dummyReceivedHTLC]
      length outs @?= 4
      case outs of
        [o1, o2, o3, o4] -> do
          B5.uo_type o1
            @?= B5.Revoke dummyRevocationPubkey
          B5.uo_type o2 @?= B5.Resolved
          case B5.uo_type o3 of
            B5.RevokeHTLC _ (OutputOfferedHTLC _) ->
              pure ()
            other -> assertFailure $
              "expected RevokeHTLC offered, got "
              <> show other
          case B5.uo_type o4 of
            B5.RevokeHTLC _ (OutputReceivedHTLC _) ->
              pure ()
            other -> assertFailure $
              "expected RevokeHTLC received, got "
              <> show other
        _ -> assertFailure "expected 4 outputs"

  , testCase "classify_local - empty commit" $ do
      let emptyCommit = CommitmentTx
            { ctx_version = 2
            , ctx_locktime = Locktime 0
            , ctx_input_outpoint = dummyOutPoint
            , ctx_input_sequence = Sequence 0
            , ctx_outputs = []
            , ctx_funding_script = dummyDestScript
            }
      B5.classify_local_commit_outputs
        emptyCommit dummyKeys dummyDelay
        dummyFeatures []
      @?= []
  ]

-- spend tests --------------------------------------------------------

spend_tests :: TestTree
spend_tests = testGroup "Spend" [
    testCase "spend_to_local produces valid tx" $ do
      let stx = B5.spend_to_local
            dummyOutPoint
            (Satoshi 100000)
            dummyRevocationPubkey
            dummyDelay
            dummyLocalDelayedPubkey
            dummyDestScript
            dummyFeerate
          tx = B5.stx_tx stx
      -- Version should be 2
      tx_version tx @?= 2
      -- Single input
      length (tx_inputs tx) @?= 1
      -- Single output
      length (tx_outputs tx) @?= 1
      -- Output value should be less than input
      let outVal = txout_value
            (head' (tx_outputs tx))
      assertBool "output < input"
        (outVal < 100000)
      -- Sighash should be ALL
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      -- Input value should match
      B5.stx_input_value stx @?= Satoshi 100000
      -- nSequence should encode delay
      let inp = head' (tx_inputs tx)
      txin_sequence inp @?= fromIntegral
        (unToSelfDelay dummyDelay)

  , testCase "spend_to_local fee deduction" $ do
      let value = Satoshi 100000
          stx = B5.spend_to_local
            dummyOutPoint value
            dummyRevocationPubkey dummyDelay
            dummyLocalDelayedPubkey
            dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
          outVal = txout_value
            (head' (tx_outputs tx))
          expectedFee = B5.spending_fee dummyFeerate
            (B5.to_local_penalty_input_weight
             + B5.penalty_tx_base_weight)
      Satoshi outVal @?=
        Satoshi (unSatoshi value
                 - unSatoshi expectedFee)

  , testCase "spend_revoked_to_local nSequence" $ do
      let stx = B5.spend_revoked_to_local
            dummyOutPoint (Satoshi 100000)
            dummyRevocationPubkey dummyDelay
            dummyLocalDelayedPubkey
            dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 0xFFFFFFFF

  , testCase "spend_anchor_owner tx structure" $ do
      let stx = B5.spend_anchor_owner
            dummyOutPoint (Satoshi 330)
            dummyFundingPubkey dummyDestScript
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      tx_locktime tx @?= 0
      B5.stx_sighash_type stx @?= SIGHASH_ALL

  , testCase "spend_anchor_anyone nSequence" $ do
      let stx = B5.spend_anchor_anyone
            dummyOutPoint (Satoshi 330)
            dummyFundingPubkey dummyDestScript
          tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 16

  , testCase "spend_htlc_output is spend_to_local" $ do
      let stx1 = B5.spend_to_local
            dummyOutPoint (Satoshi 50000)
            dummyRevocationPubkey dummyDelay
            dummyLocalDelayedPubkey
            dummyDestScript dummyFeerate
          stx2 = B5.spend_htlc_output
            dummyOutPoint (Satoshi 50000)
            dummyRevocationPubkey dummyDelay
            dummyLocalDelayedPubkey
            dummyDestScript dummyFeerate
      B5.stx_tx stx1 @?= B5.stx_tx stx2
      B5.stx_input_script stx1 @?=
        B5.stx_input_script stx2

  , testCase "spend_revoked_batch total value" $ do
      let op1 = OutPoint dummyTxId 0
          op2 = OutPoint dummyTxId 1
          uo1 = B5.UnresolvedOutput op1 (Satoshi 50000)
            (B5.Revoke dummyRevocationPubkey)
          uo2 = B5.UnresolvedOutput op2 (Satoshi 30000)
            (B5.Revoke dummyRevocationPubkey)
          pctx = B5.PenaltyContext
            { B5.pc_outputs = uo1 :| [uo2]
            , B5.pc_revocation_key =
                dummyRevocationPubkey
            , B5.pc_destination = dummyDestScript
            , B5.pc_feerate = dummyFeerate
            }
          stx = B5.spend_revoked_batch pctx
          tx = B5.stx_tx stx
          outVal = txout_value
            (head' (tx_outputs tx))
      -- Output should be less than total input
      assertBool "output < total input"
        (outVal < 80000)
      -- Should have 2 inputs
      length (tx_inputs tx) @?= 2
  ]

-- htlc spend tests ---------------------------------------------------

htlc_spend_tests :: TestTree
htlc_spend_tests = testGroup "HTLC Spend" [
    testCase "spend_htlc_timeout produces valid tx" $ do
      let stx = B5.spend_htlc_timeout
            dummyHTLCContext dummyKeys
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      length (tx_outputs tx) @?= 1
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx
        @?= msat_to_sat (MilliSatoshi 1000000)

  , testCase "spend_htlc_success produces valid tx" $ do
      let ctx = dummyHTLCContext
            { hc_htlc = dummyReceivedHTLC }
          stx = B5.spend_htlc_success ctx dummyKeys
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      B5.stx_sighash_type stx @?= SIGHASH_ALL

  , testCase "spend_htlc_timeout anchors sighash" $ do
      let ctx = dummyHTLCContext
            { hc_features = dummyFeaturesAnchors }
          stx = B5.spend_htlc_timeout ctx dummyKeys
      B5.stx_sighash_type stx
        @?= SIGHASH_SINGLE_ANYONECANPAY

  , testCase "spend_htlc_success anchors sighash" $ do
      let ctx = dummyHTLCContext
            { hc_htlc = dummyReceivedHTLC
            , hc_features = dummyFeaturesAnchors
            }
          stx = B5.spend_htlc_success ctx dummyKeys
      B5.stx_sighash_type stx
        @?= SIGHASH_SINGLE_ANYONECANPAY
  ]

-- remote spend tests -------------------------------------------------

remote_spend_tests :: TestTree
remote_spend_tests = testGroup "Remote Spend" [
    testCase "spend_remote_htlc_timeout structure" $ do
      let stx = B5.spend_remote_htlc_timeout
            dummyOutPoint (Satoshi 50000)
            dummyHTLC dummyKeys dummyFeatures
            dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx @?= Satoshi 50000
      -- locktime should be HTLC CLTV expiry
      tx_locktime tx @?= 500000

  , testCase "spend_remote_htlc_timeout fee deduction" $ do
      let value = Satoshi 50000
          stx = B5.spend_remote_htlc_timeout
            dummyOutPoint value dummyHTLC
            dummyKeys dummyFeatures
            dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
          outVal = txout_value (head' (tx_outputs tx))
      assertBool "output < input" (outVal < 50000)

  , testCase "spend_remote_htlc_preimage structure" $ do
      let stx = B5.spend_remote_htlc_preimage
            dummyOutPoint (Satoshi 50000)
            dummyReceivedHTLC dummyKeys
            dummyFeatures dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      -- locktime should be 0 for preimage claims
      tx_locktime tx @?= 0

  , testCase "spend_remote_htlc_timeout anchors seq" $
      do
      let stx = B5.spend_remote_htlc_timeout
            dummyOutPoint (Satoshi 50000)
            dummyHTLC dummyKeys dummyFeaturesAnchors
            dummyDestScript dummyFeerate
          tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 1
  ]

-- revoked htlc spend tests ------------------------------------------

revoked_htlc_spend_tests :: TestTree
revoked_htlc_spend_tests = testGroup "Revoked HTLC Spend" [
    testCase "spend_revoked_htlc - offered" $ do
      case B5.spend_revoked_htlc
            dummyOutPoint (Satoshi 50000)
            (OutputOfferedHTLC (CltvExpiry 500000))
            dummyRevocationPubkey dummyKeys
            dummyFeatures dummyPaymentHash
            dummyDestScript dummyFeerate of
        Nothing -> assertFailure "expected Just"
        Just stx -> do
          let tx = B5.stx_tx stx
          tx_version tx @?= 2
          B5.stx_sighash_type stx @?= SIGHASH_ALL
          let inp = head' (tx_inputs tx)
          txin_sequence inp @?= 0xFFFFFFFF

  , testCase "spend_revoked_htlc - received" $ do
      case B5.spend_revoked_htlc
            dummyOutPoint (Satoshi 50000)
            (OutputReceivedHTLC (CltvExpiry 500000))
            dummyRevocationPubkey dummyKeys
            dummyFeatures dummyPaymentHash
            dummyDestScript dummyFeerate of
        Nothing -> assertFailure "expected Just"
        Just stx -> do
          B5.stx_sighash_type stx @?= SIGHASH_ALL
          let tx = B5.stx_tx stx
              outVal = txout_value
                (head' (tx_outputs tx))
          assertBool "output < input"
            (outVal < 50000)

  , testCase "spend_revoked_htlc - invalid type" $ do
      B5.spend_revoked_htlc
        dummyOutPoint (Satoshi 50000)
        OutputToLocal
        dummyRevocationPubkey dummyKeys
        dummyFeatures dummyPaymentHash
        dummyDestScript dummyFeerate
      @?= Nothing
  ]

-- weight tests -------------------------------------------------------

weight_tests :: TestTree
weight_tests = testGroup "Weight" [
    testCase "penalty input = base + witness" $ do
      -- to_local: 164 (txinput) + 160 (witness) = 324
      B5.to_local_penalty_input_weight @?=
        (164 + B5.to_local_penalty_witness_weight)
      -- offered: 164 + 243 = 407
      B5.offered_htlc_penalty_input_weight @?=
        (164 + B5.offered_htlc_penalty_witness_weight)
      -- accepted: 164 + 249 = 413
      B5.accepted_htlc_penalty_input_weight @?=
        (164 + B5.accepted_htlc_penalty_witness_weight)

  , testCase "max HTLCs in single penalty tx" $ do
      -- Per spec: (400000 - 324 - 272 - 212 - 2) / 413
      -- = 399190 / 413 = 966
      let maxHtlcs = (B5.max_standard_weight
            - B5.to_local_penalty_input_weight
            - B5.to_remote_input_weight
            - B5.penalty_tx_base_weight
            - 2) `div`
            B5.accepted_htlc_penalty_input_weight
      assertBool "can sweep 483 bidirectional HTLCs"
        (maxHtlcs >= 966)
  ]

-- property tests -----------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "Properties" [
    testProperty "spending_fee always non-negative" $
      \(NonNegative rate) (NonNegative weight) ->
        let fee = B5.spending_fee
              (FeeratePerKw (fromIntegral (rate :: Int)))
              (fromIntegral (weight :: Int))
        in unSatoshi fee >= 0

  , testProperty "spend_to_local fee deduction correct" $
      \(Positive val) ->
        let value = Satoshi
              (fromIntegral (val :: Int) + 100000)
            stx = B5.spend_to_local
              dummyOutPoint value
              dummyRevocationPubkey dummyDelay
              dummyLocalDelayedPubkey
              dummyDestScript (FeeratePerKw 253)
            tx = B5.stx_tx stx
            outVal = txout_value
              (head' (tx_outputs tx))
            expectedFee = B5.spending_fee
              (FeeratePerKw 253)
              (B5.to_local_penalty_input_weight
               + B5.penalty_tx_base_weight)
        in Satoshi outVal ==
           Satoshi (unSatoshi value
                    - unSatoshi expectedFee)

  , testProperty "htlc_timed_out monotonic" $
      \(NonNegative h1) (NonNegative h2) ->
        let height1 = fromIntegral (h1 :: Int)
            height2 = fromIntegral (h2 :: Int)
            htlc = dummyHTLC
              { htlc_cltv_expiry = CltvExpiry 1000 }
        in if height1 <= height2
           then not (B5.htlc_timed_out height1 htlc)
                || B5.htlc_timed_out height2 htlc
           else True
  ]

-- helpers ------------------------------------------------------------

-- | Total head for NonEmpty.
head' :: NonEmpty a -> a
head' (x :| _) = x
