{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitcoin.Prim.Tx (TxOut(..), from_base16)
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word8, Word32, Word64)
import Lightning.Protocol.BOLT3 hiding
  (txout_value, txout_script)
import Lightning.Protocol.BOLT3.Types (Pubkey(..), PaymentHash(..))
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

-- Dummy 32-byte hash
dummyHash32 :: BS.ByteString
dummyHash32 = BS.replicate 32 0xAA

dummyTxId :: TxId
dummyTxId = TxId (BS.replicate 32 0x00)

dummyOutPoint :: OutPoint
dummyOutPoint = OutPoint dummyTxId 0

-- Dummy 33-byte pubkey. Each role gets its own key, so a key used
-- in the wrong role changes the script.
mkPubkey :: Word8 -> Pubkey
mkPubkey b = Pubkey (BS.pack (0x02 : replicate 32 b))

dummyRevocationPubkey :: RevocationPubkey
dummyRevocationPubkey = RevocationPubkey (mkPubkey 0x01)

dummyLocalDelayedPubkey :: LocalDelayedPubkey
dummyLocalDelayedPubkey = LocalDelayedPubkey (mkPubkey 0x02)

dummyLocalHtlcPubkey :: LocalHtlcPubkey
dummyLocalHtlcPubkey = LocalHtlcPubkey (mkPubkey 0x03)

dummyRemoteHtlcPubkey :: RemoteHtlcPubkey
dummyRemoteHtlcPubkey = RemoteHtlcPubkey (mkPubkey 0x04)

dummyFundingPubkey :: FundingPubkey
dummyFundingPubkey = FundingPubkey (mkPubkey 0x07)

dummyPaymentHash :: PaymentHash
dummyPaymentHash = PaymentHash dummyHash32

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
  , ck_local_payment = LocalPubkey (mkPubkey 0x05)
  , ck_remote_payment = RemotePubkey (mkPubkey 0x06)
  , ck_local_funding = dummyFundingPubkey
  , ck_remote_funding = FundingPubkey (mkPubkey 0x08)
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

-- BOLT #3 Appendix C/F vectors -------------------------------------

-- | Payment hash of an Appendix C HTLC whose preimage is 32 copies
--   of the given byte.
vecPaymentHash :: Word8 -> PaymentHash
vecPaymentHash b = PaymentHash (SHA256.hash (BS.replicate 32 b))

-- | Decode a hex string.
unhex :: BS.ByteString -> Maybe BS.ByteString
unhex bs
  | odd (BS.length bs) = Nothing
  | otherwise = BS.pack <$> go (BS.unpack bs)
  where
    go (a:b:rest) = (:) <$> byte a b <*> go rest
    go _ = Just []
    byte a b = (\hi lo -> hi * 16 + lo) <$> nibble a <*> nibble b
    nibble c
      | c >= 0x30 && c <= 0x39 = Just (c - 0x30)
      | c >= 0x61 && c <= 0x66 = Just (c - 0x57)
      | otherwise = Nothing

-- | An Appendix C HTLC, given its direction, amount (msat), expiry
--   and preimage byte.
vecHtlc :: HTLCDirection -> Word64 -> Word32 -> Word8 -> HTLC
vecHtlc dir msat expiry b = HTLC
  { htlc_direction = dir
  , htlc_amount_msat = MilliSatoshi msat
  , htlc_payment_hash = vecPaymentHash b
  , htlc_cltv_expiry = CltvExpiry expiry
  }

vecHtlc1, vecHtlc5, vecHtlc6 :: HTLC
vecHtlc1 = vecHtlc HTLCReceived 2000000 501 0x01
vecHtlc5 = vecHtlc HTLCOffered 5000000 506 0x05
vecHtlc6 = vecHtlc HTLCOffered 5000001 505 0x05

-- | Appendix C keys for the local commitment (Appendix F uses the
--   same keys).
vecKeys :: Maybe CommitmentKeys
vecKeys = do
  rev <- key
    "0212a140cd0c6539d07cd08dfe09984dec3251ea808b892efeac3ede9402bf2b19"
  delayed <- key
    "03fd5960528dc152014952efdb702a88f71e3c1653b2314431701ec77e57fde83c"
  localHtlc <- key
    "030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e7"
  remoteHtlc <- key
    "0394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b"
  localPayment <- key
    "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
  remotePayment <- key
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  localFunding <- key
    "023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"
  remoteFunding <- key
    "030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c1"
  pure CommitmentKeys
    { ck_revocation_pubkey = RevocationPubkey rev
    , ck_local_delayed = LocalDelayedPubkey delayed
    , ck_local_htlc = LocalHtlcPubkey localHtlc
    , ck_remote_htlc = RemoteHtlcPubkey remoteHtlc
    , ck_local_payment = LocalPubkey localPayment
    , ck_remote_payment = RemotePubkey remotePayment
    , ck_local_funding = FundingPubkey localFunding
    , ck_remote_funding = FundingPubkey remoteFunding
    }
  where
    key h = unhex h >>= pubkey

-- | Rebuild an Appendix C/F local commitment with bolt3, given the
--   feerate, channel features, to_local balance and HTLCs.
vecCommitment
  :: FeeratePerKw -> ChannelFeatures -> MilliSatoshi -> [HTLC]
  -> Maybe CommitmentTx
vecCommitment feerate features toLocal htlcs = do
  keys <- vecKeys
  fundingTxid <- mkTxId =<< unhex
    "bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489"
  cn <- commitment_number 42
  localBp <- point =<< unhex
    "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
  remoteBp <- point =<< unhex
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  pure $ build_commitment_tx CommitmentContext
    { cc_funding_outpoint = OutPoint fundingTxid 0
    , cc_commitment_number = cn
    , cc_local_payment_bp = PaymentBasepoint localBp
    , cc_remote_payment_bp = PaymentBasepoint remoteBp
    , cc_to_self_delay = ToSelfDelay 144
    , cc_dust_limit = DustLimit (Satoshi 546)
    , cc_feerate = feerate
    , cc_features = features
    , cc_is_funder = True
    , cc_to_local_msat = toLocal
    , cc_to_remote_msat = MilliSatoshi 3000000000
    , cc_htlcs = htlcs
    , cc_keys = keys
    }

-- | The Appendix C commitment with two offered HTLCs sharing a
--   payment hash and amount.
vecCommitSameHash :: Maybe CommitmentTx
vecCommitSameHash = vecCommitment (FeeratePerKw 253)
  (ChannelFeatures False) (MilliSatoshi 6987999999)
  [vecHtlc1, vecHtlc5, vecHtlc6]

-- | Appendix C: commitment tx with 3 htlc outputs, 2 offered
--   having the same amount and preimage (signed).
vecCCommitSameHash :: BS.ByteString
vecCCommitSameHash =
  "02000000000101bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b\
  \820b584a488489000000000038b02b8005d007000000000000220020748eba94\
  \4fedc8827f6b06bc44678f93c0f9e6078b35c6331ed31e75f8ce0c2d88130000\
  \00000000220020305c12e1a0bc21e283c131cea1c66d68857d28b7b2fce0a6fb\
  \c40c164852121b8813000000000000220020305c12e1a0bc21e283c131cea1c6\
  \6d68857d28b7b2fce0a6fbc40c164852121bc0c62d0000000000160014cc1b07\
  \838e387deacd0e5232e1e8b49f4c29e484a69f6a00000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e04004730\
  \4402200d10bf5bc5397fc59d7188ae438d80c77575595a2d488e41bd6363a810\
  \cc8d72022012b57e714fbbfdf7a28c47d5b370cb8ac37c8545f596216e5b21e9\
  \b236ef457c0147304402207d0870964530f97b62497b11153c551dca0a1e2268\
  \15ef0a336651158da0f82402200f5378beee0e77759147b8a0a284decd11bfd2\
  \bc55c8fafa41c134fe996d43c801475221023da092f6980e58d2c037173180e9\
  \a465476026ee50f96695963e8efe436f54eb21030e9f7b623d2ccc7c9bd44d66\
  \d5ce21ce504c0acf6385a132cec6d3c39fa711c152ae3e195220"

-- | Appendix C: HTLC-success tx for htlc #1 from the same
--   commitment (signed).
vecCHtlcSuccess1 :: BS.ByteString
vecCHtlcSuccess1 =
  "020000000001014bdccf28653066a2c554cafeffdfe1e678e64a69b056684deb\
  \0c4fba909423ec000000000000000000011f070000000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004830\
  \45022100b470fe12e5b7fea9eccb8cbff1972cea4f96758041898982a02bcc7f\
  \9d56d50b0220338a75b2afaab4ec00cdd2d9273c68c7581ff5a28bcbb40c4d13\
  \8b81f1d45ce501473044022017b90c65207522a907fb6a137f9dd528b3389465\
  \a8ae72308d9e1d564f512cf402204fc917b4f0e88604a3e994f85bfae7c7c1f9\
  \d9e9f78e8cd112e0889720d9405b012001010101010101010101010101010101\
  \010101010101010101010101010101018a76a91414011f7254d96b819c76986c\
  \277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a21\
  \84d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb\
  \7bcea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e5797\
  \65875dc4daca813e21734b140639e752ae677502f501b175ac686800000000"

-- | Appendix C: HTLC-timeout tx for htlc #5 from the same
--   commitment (signed).
vecCHtlcTimeout5 :: BS.ByteString
vecCHtlcTimeout5 =
  "020000000001014bdccf28653066a2c554cafeffdfe1e678e64a69b056684deb\
  \0c4fba909423ec02000000000000000001e1120000000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004730\
  \440220471c9f3ad92e49b13b7b8059f43ecf8f7887b0dccbb9fdb54bfe23d62a\
  \8ae332022024bd22fae0740e86a44228c35330da9526fd7306dffb2b9dc362d5\
  \e78abef7cc0147304402207157f452f2506d73c315192311893800cfb3cc235c\
  \c1185b1cfcc136b55230db022014be242dbc6c5da141fec4034e7f387f74d6ff\
  \1899453d72ba957467540e1ecb01008576a91414011f7254d96b819c76986c27\
  \7d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184\
  \d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384f8\
  \8d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a9142002cc\
  \93ebefbb1b73f0af055dcc27a0b504ad7688ac6868fa010000"

-- | Appendix F: HTLC-success tx for htlc #1 from "commitment tx
--   with seven outputs untrimmed" (signed).
vecFHtlcSuccess1 :: BS.ByteString
vecFHtlcSuccess1 =
  "02000000000101b8cefef62ea66f5178b9361b2371be0759cbc8c689bcfa7a8e\
  \6746d497ec221a04000000000100000001d0070000000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004730\
  \44022035977f2aa6d6ae12f1dae3366440faa17558e9c88c3188bfc8df7e276c\
  \6c65410220659f1f9070c725d9d46e43b99a36fc6d711d36069704add2d57fc1\
  \fa2818cf12834730440220191ba44e57b1601a59d99f85971d4801b286d428de\
  \275487c87ceeb8df1a4811022002215875d92833df0c9615c9096cf97152f871\
  \39ed9f82718bbb5b8b3d31252401200101010101010101010101010101010101\
  \0101010101010101010101010101018d76a91414011f7254d96b819c76986c27\
  \7d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a2184\
  \d88256816826d6231c068d4a5b7c8201208763a9144b6b2e5444c2639cc0fb7b\
  \cea5afba3f3cdce23988527c21030d417a46946384f88d5f3337267c5e579765\
  \875dc4daca813e21734b140639e752ae677502f501b175ac6851b27568000000\
  \00"

-- | Parse a hex-encoded transaction, failing the test otherwise.
parseTx :: BS.ByteString -> IO Tx
parseTx = assertJust "invalid transaction hex" . from_base16

-- | Witness stack of a single-input transaction.
singleWitness :: Tx -> IO [BS.ByteString]
singleWitness tx = case tx_witnesses tx of
  [Witness items] -> pure items
  _ -> assertFailure "expected a single witness"

-- | Remote signature and offered HTLC witness script, taken from the
--   Appendix C HTLC-timeout tx for htlc #5.
offeredClaimParts :: IO (BS.ByteString, BS.ByteString)
offeredClaimParts = do
  items <- singleWitness =<< parseTx vecCHtlcTimeout5
  case items of
    [_, sig, _, _, script] -> pure (sig, script)
    _ -> assertFailure "unexpected HTLC-timeout witness"

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
    testCase "extract_preimage_htlc_success - Appendix C" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      fmap unPaymentPreimage
        (B5.extract_preimage_htlc_success
          (vecPaymentHash 0x01) (Witness items))
        @?= Just (BS.replicate 32 0x01)

  , testCase "extract_preimage_htlc_success - Appendix F" $ do
      items <- singleWitness =<< parseTx vecFHtlcSuccess1
      fmap unPaymentPreimage
        (B5.extract_preimage_htlc_success
          (vecPaymentHash 0x01) (Witness items))
        @?= Just (BS.replicate 32 0x01)

  , testCase "extract_preimage_htlc_success - wrong hash" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      B5.extract_preimage_htlc_success
        (vecPaymentHash 0x05) (Witness items)
        @?= Nothing

  , testCase "extract_preimage_htlc_success - timeout witness" $ do
      items <- singleWitness =<< parseTx vecCHtlcTimeout5
      B5.extract_preimage_htlc_success
        (vecPaymentHash 0x05) (Witness items)
        @?= Nothing

  , testCase "extract_preimage_htlc_success - no script" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      B5.extract_preimage_htlc_success
        (vecPaymentHash 0x01) (Witness (take 4 items))
        @?= Nothing

  , testCase "extract_preimage_offered - remote claim" $ do
      -- <remotehtlcsig> <payment_preimage> <witness_script>; the
      -- signature is not checked.
      (sig, script) <- offeredClaimParts
      let preimage = BS.replicate 32 0x05
      fmap unPaymentPreimage
        (B5.extract_preimage_offered (vecPaymentHash 0x05)
          (Witness [sig, preimage, script]))
        @?= Just preimage

  , testCase "extract_preimage_offered - wrong hash" $ do
      (sig, script) <- offeredClaimParts
      B5.extract_preimage_offered (vecPaymentHash 0x01)
        (Witness [sig, BS.replicate 32 0x05, script])
        @?= Nothing

  , testCase "extract_preimage_offered - revocation spend" $ do
      -- <revocation_sig> <revocationpubkey> <witness_script>
      (sig, script) <- offeredClaimParts
      B5.extract_preimage_offered (vecPaymentHash 0x05)
        (Witness [sig, unPubkey (mkPubkey 0x01), script])
        @?= Nothing

  , testCase "extract_preimage_offered - no script" $ do
      (sig, _) <- offeredClaimParts
      B5.extract_preimage_offered (vecPaymentHash 0x05)
        (Witness [sig, BS.replicate 32 0x05])
        @?= Nothing

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
    testCase "identify_close - signed local commitment" $ do
      commit <- assertJust "commitment" vecCommitSameHash
      signed <- assertJust "hex" (unhex vecCCommitSameHash)
      B5.identify_close commit dummyRemoteCommitTx signed
        @?= Just B5.LocalCommitClose

  , testCase "identify_close - signed remote commitment" $ do
      commit <- assertJust "commitment" vecCommitSameHash
      signed <- assertJust "hex" (unhex vecCCommitSameHash)
      B5.identify_close dummyLocalCommitTx commit signed
        @?= Just B5.RemoteCommitClose

  , testCase "identify_close - unsigned commitment" $ do
      commit <- assertJust "commitment" vecCommitSameHash
      unsigned <- assertJust "encode" (encode_tx_for_signing commit)
      B5.identify_close commit dummyRemoteCommitTx unsigned
        @?= Just B5.LocalCommitClose

  , testCase "identify_close - no match" $ do
      signed <- assertJust "hex" (unhex vecCCommitSameHash)
      B5.identify_close
        dummyLocalCommitTx dummyRemoteCommitTx signed
        @?= Nothing

  , testCase "identify_close - unparseable" $ do
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
            B5.RevokeHTLC _ (B5.HTLCOfferedOutput _) ->
              pure ()
            other -> assertFailure $
              "expected RevokeHTLC offered, got "
              <> show other
          case B5.uo_type o4 of
            B5.RevokeHTLC _ (B5.HTLCReceivedOutput _) ->
              pure ()
            other -> assertFailure $
              "expected RevokeHTLC received, got "
              <> show other
        _ -> assertFailure "expected 4 outputs"

  , testCase "classify_local - anchor outputs" $ do
      let anchorScript = to_p2wsh $
            anchor_script dummyFundingPubkey
          remoteFundPk = ck_remote_funding dummyKeys
          remoteAnchorScript = to_p2wsh $
            anchor_script remoteFundPk
          commitTx = CommitmentTx
            { ctx_version = 2
            , ctx_locktime = Locktime 0
            , ctx_input_outpoint = dummyOutPoint
            , ctx_input_sequence = Sequence 0
            , ctx_outputs =
                [ TxOutput (Satoshi 330) anchorScript
                    OutputLocalAnchor
                , TxOutput (Satoshi 330) remoteAnchorScript
                    OutputRemoteAnchor
                ]
            , ctx_funding_script = dummyDestScript
            }
          outs = B5.classify_local_commit_outputs
            commitTx dummyKeys dummyDelay
            dummyFeaturesAnchors []
      length outs @?= 2
      case outs of
        [o1, o2] -> do
          case B5.uo_type o1 of
            B5.AnchorSpend _ -> pure ()
            other -> assertFailure $
              "expected AnchorSpend, got "
              <> show other
          B5.uo_type o2 @?= B5.Resolved
        _ -> assertFailure "expected 2 outputs"

  , testCase "classify_remote - non-HTLC outputs" $ do
      let outs = B5.classify_remote_commit_outputs
            dummyRemoteCommitTx dummyKeys
            dummyFeatures []
      length outs @?= 2
      case outs of
        [o1, o2] -> do
          B5.uo_type o1 @?= B5.Resolved
          B5.uo_type o2 @?= B5.Resolved
        _ -> assertFailure "expected 2 outputs"

  , testCase "classify_local - unmatched HTLC" $ do
      let outs = B5.classify_local_commit_outputs
            dummyLocalCommitWithHTLCs dummyKeys
            dummyDelay dummyFeatures []
      -- With no HTLCs passed, HTLC outputs -> Resolved
      length outs @?= 4
      case outs of
        [_, _, o3, o4] -> do
          B5.uo_type o3 @?= B5.Resolved
          B5.uo_type o4 @?= B5.Resolved
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
      stx <- assertJust "spend_to_local" $
        B5.spend_to_local
          dummyOutPoint
          (Satoshi 100000)
          dummyRevocationPubkey
          dummyDelay
          dummyLocalDelayedPubkey
          dummyDestScript
          dummyFeerate
      let tx = B5.stx_tx stx
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
      stx <- assertJust "spend_to_local" $
        B5.spend_to_local
          dummyOutPoint value
          dummyRevocationPubkey dummyDelay
          dummyLocalDelayedPubkey
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
          outVal = txout_value
            (head' (tx_outputs tx))
          expectedFee = B5.spending_fee dummyFeerate
            (B5.to_local_penalty_input_weight
             + B5.penalty_tx_base_weight)
      Satoshi outVal @?=
        Satoshi (unSatoshi value
                 - unSatoshi expectedFee)

  , testCase "spend_revoked_to_local nSequence" $ do
      stx <- assertJust "spend_revoked_to_local" $
        B5.spend_revoked_to_local
          dummyOutPoint (Satoshi 100000)
          dummyRevocationPubkey dummyDelay
          dummyLocalDelayedPubkey
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
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
      stx1 <- assertJust "spend_to_local" $
        B5.spend_to_local
          dummyOutPoint (Satoshi 50000)
          dummyRevocationPubkey dummyDelay
          dummyLocalDelayedPubkey
          dummyDestScript dummyFeerate
      stx2 <- assertJust "spend_htlc_output" $
        B5.spend_htlc_output
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
          ro1 = B5.RevokedOutput op1 (Satoshi 50000)
            B5.RevokedToLocal
          ro2 = B5.RevokedOutput op2 (Satoshi 30000)
            B5.RevokedToLocal
          pctx = B5.PenaltyContext
            { B5.pc_outputs = ro1 :| [ro2]
            , B5.pc_revocation_key =
                dummyRevocationPubkey
            , B5.pc_destination = dummyDestScript
            , B5.pc_feerate = dummyFeerate
            }
      stx <- assertJust "spend_revoked_batch" $
        B5.spend_revoked_batch pctx
      let tx = B5.stx_tx stx
          outVal = txout_value
            (head' (tx_outputs tx))
      -- Output should be less than total input
      assertBool "output < total input"
        (outVal < 80000)
      -- Should have 2 inputs
      length (tx_inputs tx) @?= 2

  , testCase "spend_revoked_batch single element" $ do
      let ro = B5.RevokedOutput
            dummyOutPoint (Satoshi 50000)
            B5.RevokedToLocal
          pctx = B5.PenaltyContext
            { B5.pc_outputs = ro :| []
            , B5.pc_revocation_key =
                dummyRevocationPubkey
            , B5.pc_destination = dummyDestScript
            , B5.pc_feerate = dummyFeerate
            }
      stx <- assertJust "spend_revoked_batch" $
        B5.spend_revoked_batch pctx
      let tx = B5.stx_tx stx
      length (tx_inputs tx) @?= 1
      length (tx_outputs tx) @?= 1

  , testCase "spend_revoked_batch mixed types" $ do
      let op1 = OutPoint dummyTxId 0
          op2 = OutPoint dummyTxId 1
          op3 = OutPoint dummyTxId 2
          ro1 = B5.RevokedOutput op1
            (Satoshi 50000)
            B5.RevokedToLocal
          ro2 = B5.RevokedOutput op2
            (Satoshi 10000)
            (B5.RevokedHTLC
              (B5.HTLCOfferedOutput
                (CltvExpiry 500000)))
          ro3 = B5.RevokedOutput op3
            (Satoshi 10000)
            (B5.RevokedHTLC
              (B5.HTLCReceivedOutput
                (CltvExpiry 500000)))
          pctx = B5.PenaltyContext
            { B5.pc_outputs = ro1 :| [ro2, ro3]
            , B5.pc_revocation_key =
                dummyRevocationPubkey
            , B5.pc_destination = dummyDestScript
            , B5.pc_feerate = dummyFeerate
            }
      stx <- assertJust "spend_revoked_batch" $
        B5.spend_revoked_batch pctx
      let tx = B5.stx_tx stx
      length (tx_inputs tx) @?= 3

  , testCase "spend_to_local fee underflow" $ do
      B5.spend_to_local
        dummyOutPoint (Satoshi 1)
        dummyRevocationPubkey dummyDelay
        dummyLocalDelayedPubkey
        dummyDestScript dummyFeerate
      @?= Nothing

  , testCase "spend_revoked_batch fee underflow" $ do
      let ro = B5.RevokedOutput
            dummyOutPoint (Satoshi 1)
            B5.RevokedToLocal
          pctx = B5.PenaltyContext
            { B5.pc_outputs = ro :| []
            , B5.pc_revocation_key =
                dummyRevocationPubkey
            , B5.pc_destination = dummyDestScript
            , B5.pc_feerate = dummyFeerate
            }
      B5.spend_revoked_batch pctx @?= Nothing
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
        @?= msatToSat (MilliSatoshi 1000000)

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
      stx <- assertJust "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout
          dummyOutPoint (Satoshi 50000)
          dummyHTLC dummyKeys dummyFeatures
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx @?= Satoshi 50000
      -- locktime should be HTLC CLTV expiry
      tx_locktime tx @?= 500000

  , testCase "spend_remote_htlc_timeout fee deduction" $ do
      let value = Satoshi 50000
      stx <- assertJust "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout
          dummyOutPoint value dummyHTLC
          dummyKeys dummyFeatures
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
          outVal = txout_value (head' (tx_outputs tx))
      assertBool "output < input" (outVal < 50000)

  , testCase "spend_remote_htlc_preimage structure" $ do
      stx <- assertJust "spend_remote_htlc_preimage" $
        B5.spend_remote_htlc_preimage
          dummyOutPoint (Satoshi 50000)
          dummyReceivedHTLC dummyKeys
          dummyFeatures dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      -- locktime should be 0 for preimage claims
      tx_locktime tx @?= 0

  , testCase "spend_remote_htlc_timeout anchors seq" $
      do
      stx <- assertJust "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout
          dummyOutPoint (Satoshi 50000)
          dummyHTLC dummyKeys dummyFeaturesAnchors
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 1

  , testCase "spend_remote_htlc_timeout no-anchor seq" $
      do
      stx <- assertJust "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout
          dummyOutPoint (Satoshi 50000)
          dummyHTLC dummyKeys dummyFeatures
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 0

  , testCase "spend_remote_htlc_preimage nSequence" $ do
      stx <- assertJust "spend_remote_htlc_preimage" $
        B5.spend_remote_htlc_preimage
          dummyOutPoint (Satoshi 50000)
          dummyReceivedHTLC dummyKeys
          dummyFeatures dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
          inp = head' (tx_inputs tx)
      txin_sequence inp @?= 0
  ]

-- revoked htlc spend tests ------------------------------------------

revoked_htlc_spend_tests :: TestTree
revoked_htlc_spend_tests = testGroup "Revoked HTLC Spend" [
    testCase "spend_revoked_htlc - offered" $ do
      case B5.spend_revoked_htlc
            dummyOutPoint (Satoshi 50000)
            (B5.HTLCOfferedOutput (CltvExpiry 500000))
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
            (B5.HTLCReceivedOutput
              (CltvExpiry 500000))
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

  , testCase "htlcOutputType - valid HTLC types" $ do
      B5.htlcOutputType
        (OutputOfferedHTLC (CltvExpiry 500))
        @?= Just (B5.HTLCOfferedOutput (CltvExpiry 500))
      B5.htlcOutputType
        (OutputReceivedHTLC (CltvExpiry 600))
        @?= Just (B5.HTLCReceivedOutput
                    (CltvExpiry 600))

  , testCase "htlcOutputType - non-HTLC types" $ do
      B5.htlcOutputType OutputToLocal @?= Nothing
      B5.htlcOutputType OutputToRemote @?= Nothing
      B5.htlcOutputType OutputLocalAnchor @?= Nothing
      B5.htlcOutputType OutputRemoteAnchor @?= Nothing

  , testCase "spend_revoked_htlc_output structure" $ do
      stx <- assertJust "spend_revoked_htlc_output" $
        B5.spend_revoked_htlc_output
          dummyOutPoint (Satoshi 50000)
          dummyRevocationPubkey dummyDelay
          dummyLocalDelayedPubkey
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      let inp = head' (tx_inputs tx)
      txin_sequence inp @?= 0xFFFFFFFF
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
            expectedFee = B5.spending_fee
              (FeeratePerKw 253)
              (B5.to_local_penalty_input_weight
               + B5.penalty_tx_base_weight)
        in case B5.spend_to_local
                  dummyOutPoint value
                  dummyRevocationPubkey dummyDelay
                  dummyLocalDelayedPubkey
                  dummyDestScript (FeeratePerKw 253) of
             Nothing -> False
             Just stx ->
               let tx = B5.stx_tx stx
                   outVal = txout_value
                     (head' (tx_outputs tx))
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

  , testProperty "output + fee = input (spend_to_local)" $
      \(Positive val) ->
        let value = Satoshi
              (fromIntegral (val :: Int) + 100000)
            fee = B5.spending_fee (FeeratePerKw 253)
              (B5.to_local_penalty_input_weight
               + B5.penalty_tx_base_weight)
        in case B5.spend_to_local
                  dummyOutPoint value
                  dummyRevocationPubkey dummyDelay
                  dummyLocalDelayedPubkey
                  dummyDestScript (FeeratePerKw 253) of
             Nothing -> False
             Just stx ->
               let tx = B5.stx_tx stx
                   outVal = txout_value
                     (head' (tx_outputs tx))
               in outVal + unSatoshi fee ==
                  unSatoshi value

  , testProperty "fee monotonic in feerate" $
      \(Positive r1) (Positive r2) ->
        let rate1 = fromIntegral (r1 :: Int)
            rate2 = fromIntegral (r2 :: Int)
            w = B5.to_local_penalty_input_weight
              + B5.penalty_tx_base_weight
            fee1 = B5.spending_fee
              (FeeratePerKw rate1) w
            fee2 = B5.spending_fee
              (FeeratePerKw rate2) w
        in if rate1 <= rate2
           then unSatoshi fee1 <= unSatoshi fee2
           else unSatoshi fee1 >= unSatoshi fee2

  , testProperty "classify preserves output count" $
      \(NonNegative n') ->
        let n = min (n' :: Int) 10
            outputs =
              [ TxOutput (Satoshi 50000) toLocalScript
                  OutputToLocal
              | _ <- [1..n] ]
            commitTx = CommitmentTx
              { ctx_version = 2
              , ctx_locktime = Locktime 0
              , ctx_input_outpoint = dummyOutPoint
              , ctx_input_sequence = Sequence 0
              , ctx_outputs = outputs
              , ctx_funding_script = dummyDestScript
              }
            outs = B5.classify_local_commit_outputs
              commitTx dummyKeys dummyDelay
              dummyFeatures []
        in length outs == n

  , testProperty "spend_to_local always version 2" $
      \(Positive val) ->
        let value = Satoshi
              (fromIntegral (val :: Int) + 100000)
        in case B5.spend_to_local
                  dummyOutPoint value
                  dummyRevocationPubkey dummyDelay
                  dummyLocalDelayedPubkey
                  dummyDestScript (FeeratePerKw 253) of
             Nothing -> False
             Just stx ->
               tx_version (B5.stx_tx stx) == 2

  , testProperty "revoked nSequence is 0xFFFFFFFF" $
      \(Positive val) ->
        let value = Satoshi
              (fromIntegral (val :: Int) + 100000)
        in case B5.spend_revoked_to_local
                  dummyOutPoint value
                  dummyRevocationPubkey dummyDelay
                  dummyLocalDelayedPubkey
                  dummyDestScript (FeeratePerKw 253) of
             Nothing -> False
             Just stx ->
               let inp = head'
                     (tx_inputs (B5.stx_tx stx))
               in txin_sequence inp == 0xFFFFFFFF

  , testProperty "htlcOutputType preserves expiry" $
      \expiry ->
        let ce = CltvExpiry expiry
        in  B5.htlcOutputType (OutputOfferedHTLC ce)
            == Just (B5.HTLCOfferedOutput ce)
            &&
            B5.htlcOutputType (OutputReceivedHTLC ce)
            == Just (B5.HTLCReceivedOutput ce)

  , testProperty "htlcOutputType rejects non-HTLC" $
      property $
        B5.htlcOutputType OutputToLocal == Nothing
        && B5.htlcOutputType OutputToRemote == Nothing
        && B5.htlcOutputType OutputLocalAnchor == Nothing
        && B5.htlcOutputType OutputRemoteAnchor
           == Nothing

  , testProperty "revoked_output_weight by type" $
      \expiry ->
        let ce = CltvExpiry expiry
            roLocal = B5.RevokedOutput
              dummyOutPoint (Satoshi 1000)
              B5.RevokedToLocal
            roOffered = B5.RevokedOutput
              dummyOutPoint (Satoshi 1000)
              (B5.RevokedHTLC
                (B5.HTLCOfferedOutput ce))
            roReceived = B5.RevokedOutput
              dummyOutPoint (Satoshi 1000)
              (B5.RevokedHTLC
                (B5.HTLCReceivedOutput ce))
        in  B5.revoked_output_weight roLocal
            == B5.to_local_penalty_input_weight
            &&
            B5.revoked_output_weight roOffered
            == B5.offered_htlc_penalty_input_weight
            &&
            B5.revoked_output_weight roReceived
            == B5.accepted_htlc_penalty_input_weight

  , testProperty "spend_revoked_batch input count" $
      \(Positive n') ->
        let n = min (n' :: Int) 5
            ros = [ B5.RevokedOutput
                      (OutPoint dummyTxId
                        (fromIntegral i))
                      (Satoshi 1000000)
                      B5.RevokedToLocal
                  | i <- [0..n-1] ]
        in case ros of
          [] -> True  -- impossible with Positive
          (r:rs) ->
            let pctx = B5.PenaltyContext
                  { B5.pc_outputs = r :| rs
                  , B5.pc_revocation_key =
                      dummyRevocationPubkey
                  , B5.pc_destination =
                      dummyDestScript
                  , B5.pc_feerate = FeeratePerKw 253
                  }
            in case B5.spend_revoked_batch pctx of
              Nothing -> False
              Just stx ->
                length (tx_inputs (B5.stx_tx stx))
                == n
  ]

-- helpers ------------------------------------------------------------

-- | Total head for NonEmpty.
head' :: NonEmpty a -> a
head' (x :| _) = x

-- | Assert a Maybe is Just, failing with a message otherwise.
assertJust :: String -> Maybe a -> IO a
assertJust msg = maybe (assertFailure msg) pure
