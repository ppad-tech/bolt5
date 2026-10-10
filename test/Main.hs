{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bitcoin.Prim.Tx (OutPoint(..), Tx(..), TxId, TxIn(..),
                        TxOut(..), Witness(..))
import qualified Bitcoin.Prim.Tx as BT
import Bitcoin.Prim.Tx.Sighash (SighashType(..), encode_sighash)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word8, Word32, Word64)
import Lightning.Protocol.BOLT1 (MilliSatoshi, PaymentHash, Satoshi)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3
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

-- helpers ------------------------------------------------------------

-- | Unwrap a Just, failing the enclosing test on Nothing.
need :: String -> Maybe a -> IO a
need msg = maybe (assertFailure msg) pure

sat :: Word64 -> IO Satoshi
sat = need "invalid satoshi amount" . BOLT1.satoshi

msat :: Word64 -> IO MilliSatoshi
msat = need "invalid millisatoshi amount" . BOLT1.milli_satoshi

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

-- | A point from hex.
pt :: BS.ByteString -> IO BOLT1.Point
pt h = need ("invalid point " <> show h) (unhex h >>= BOLT1.point)

-- | Total head for NonEmpty.
head' :: NonEmpty a -> a
head' (x :| _) = x

-- | Value of a spending transaction's single output.
out_value :: B5.SpendingTx -> Word64
out_value = txout_value . head' . tx_outputs . B5.stx_tx

-- | nSequence of a spending transaction's single input.
in_sequence :: B5.SpendingTx -> Word32
in_sequence = txin_sequence . head' . tx_inputs . B5.stx_tx

-- fixtures -----------------------------------------------------------

-- | Fixtures shared by the unit and property tests. Each key role
--   gets its own point, so a key used in the wrong role changes the
--   script.
data Fx = Fx
  { fx_revpk         :: !RevocationPubkey
  , fx_delayed       :: !LocalDelayedPubkey
  , fx_local_htlc    :: !LocalHtlcPubkey
  , fx_remote_htlc   :: !RemoteHtlcPubkey
  , fx_funding       :: !FundingPubkey
  , fx_keys          :: !CommitmentKeys
  , fx_hash          :: !PaymentHash
  , fx_offered       :: !HTLC
    -- ^ offered, 1000 sat, expiry 500000
  , fx_received      :: !HTLC
    -- ^ as 'fx_offered', but received
  , fx_local_commit  :: !CommitmentTx
    -- ^ to_local and to_remote outputs
  , fx_htlc_commit   :: !CommitmentTx
    -- ^ as 'fx_local_commit', plus both HTLCs
  , fx_remote_commit :: !CommitmentTx
    -- ^ distinct from 'fx_local_commit'
  }

fixtures :: Maybe Fx
fixtures = do
  let point b = BOLT1.point (BS.pack (0x02 : replicate 32 b))
  revpk <- RevocationPubkey <$> point 0x01
  delayed <- LocalDelayedPubkey <$> point 0x02
  local_htlc <- LocalHtlcPubkey <$> point 0x03
  remote_htlc <- RemoteHtlcPubkey <$> point 0x04
  remote_payment <- RemotePubkey <$> point 0x06
  funding <- FundingPubkey <$> point 0x07
  remote_funding <- FundingPubkey <$> point 0x08
  ph <- BOLT1.payment_hash (BS.replicate 32 0xAA)
  amt <- BOLT1.milli_satoshi 1000000
  v1000 <- BOLT1.satoshi 1000
  v20k <- BOLT1.satoshi 20000
  v30k <- BOLT1.satoshi 30000
  v40k <- BOLT1.satoshi 40000
  v50k <- BOLT1.satoshi 50000
  let keys = CommitmentKeys
        { ck_revocation_pubkey = revpk
        , ck_local_delayed     = delayed
        , ck_local_htlc        = local_htlc
        , ck_remote_htlc       = remote_htlc
        , ck_remote_payment    = remote_payment
        , ck_local_funding     = funding
        , ck_remote_funding    = remote_funding
        }
      offered = HTLC
        { htlc_direction    = HTLCOffered
        , htlc_amount_msat  = amt
        , htlc_payment_hash = ph
        , htlc_cltv_expiry  = CltvExpiry 500000
        }
      received = offered { htlc_direction = HTLCReceived }
      to_local = to_p2wsh (to_local_script revpk dummyDelay delayed)
      htlc_out h = CommitmentOutput v1000
        (to_p2wsh (htlc_witness_script keys StaticRemotekey h))
        (OutputHTLC h)
      commit lt outs = CommitmentTx
        { ctx_version        = 2
        , ctx_locktime       = Locktime lt
        , ctx_input_outpoint = dummyOutPoint
        , ctx_input_sequence = Sequence 0
        , ctx_outputs        = outs
        , ctx_funding_script = dummyDestScript
        }
      local_outs =
           CommitmentOutput v50k to_local OutputToLocal
        :| [CommitmentOutput v30k dummyDestScript OutputToRemote]
  pure Fx
    { fx_revpk         = revpk
    , fx_delayed       = delayed
    , fx_local_htlc    = local_htlc
    , fx_remote_htlc   = remote_htlc
    , fx_funding       = funding
    , fx_keys          = keys
    , fx_hash          = ph
    , fx_offered       = offered
    , fx_received      = received
    , fx_local_commit  = commit 0 local_outs
    , fx_htlc_commit   = commit 0 $ local_outs
        <> (htlc_out offered :| [htlc_out received])
    , fx_remote_commit = commit 1 $
           CommitmentOutput v40k to_local OutputToLocal
        :| [CommitmentOutput v20k dummyDestScript OutputToRemote]
    }

-- | Run a test with the fixtures.
with_fx :: (Fx -> Assertion) -> Assertion
with_fx k = need "invalid fixtures" fixtures >>= k

-- | A property over the fixtures.
prop_fx :: Testable p => (Fx -> p) -> Property
prop_fx k = case fixtures of
  Nothing -> counterexample "invalid fixtures" False
  Just fx -> property (k fx)

dummyTxId :: TxId
dummyTxId = BT.null_txid

dummyOutPoint :: OutPoint
dummyOutPoint = OutPoint dummyTxId 0

dummyDelay :: ToSelfDelay
dummyDelay = ToSelfDelay 144

dummyFeerate :: FeeratePerKw
dummyFeerate = FeeratePerKw 253

-- | A P2WPKH scriptPubKey.
dummyDestScript :: Script
dummyDestScript = Script $ BS.pack [0x00, 0x14] <> BS.replicate 20 0xCC

-- | The witness script of an HTLC output of a commitment with the
--   given keys.
htlc_witness_script :: CommitmentKeys -> CommitmentFormat -> HTLC -> Script
htlc_witness_script keys fmt h = case htlc_direction h of
  HTLCOffered -> offered_htlc_script (ck_revocation_pubkey keys)
    (ck_remote_htlc keys) (ck_local_htlc keys) (htlc_payment_hash h) fmt
  HTLCReceived -> received_htlc_script (ck_revocation_pubkey keys)
    (ck_remote_htlc keys) (ck_local_htlc keys) (htlc_payment_hash h)
    (htlc_cltv_expiry h) fmt

-- | An HTLC context for one of the fixture HTLCs.
htlc_context :: Fx -> HTLC -> CommitmentFormat -> HTLCContext
htlc_context fx h fmt = HTLCContext
  { hc_commitment_txid   = dummyTxId
  , hc_output_index      = 0
  , hc_htlc              = h
  , hc_to_self_delay     = dummyDelay
  , hc_feerate           = dummyFeerate
  , hc_format            = fmt
  , hc_revocation_pubkey = fx_revpk fx
  , hc_local_delayed     = fx_delayed fx
  }

-- BOLT #3 Appendix C/F vectors -------------------------------------

-- | Payment hash of an Appendix C HTLC whose preimage is 32 copies
--   of the given byte.
vecPaymentHash :: Word8 -> IO PaymentHash
vecPaymentHash b =
  need "payment hash" (BOLT1.payment_hash (SHA256.hash (BS.replicate 32 b)))

-- | An Appendix C HTLC, given its direction, amount (msat), expiry
--   and preimage byte.
vecHtlc :: HTLCDirection -> Word64 -> Word32 -> Word8 -> IO HTLC
vecHtlc dir amt expiry b = do
  a <- msat amt
  ph <- vecPaymentHash b
  pure HTLC
    { htlc_direction    = dir
    , htlc_amount_msat  = a
    , htlc_payment_hash = ph
    , htlc_cltv_expiry  = CltvExpiry expiry
    }

vecHtlc0, vecHtlc1, vecHtlc2, vecHtlc3, vecHtlc4 :: IO HTLC
vecHtlc0 = vecHtlc HTLCReceived 1000000 500 0x00
vecHtlc1 = vecHtlc HTLCReceived 2000000 501 0x01
vecHtlc2 = vecHtlc HTLCOffered 2000000 502 0x02
vecHtlc3 = vecHtlc HTLCOffered 3000000 503 0x03
vecHtlc4 = vecHtlc HTLCReceived 4000000 504 0x04

vecHtlc5, vecHtlc6 :: IO HTLC
vecHtlc5 = vecHtlc HTLCOffered 5000000 506 0x05
vecHtlc6 = vecHtlc HTLCOffered 5000001 505 0x05

-- | Appendix C keys for the local commitment (Appendix F uses the
--   same keys).
vecKeys :: IO CommitmentKeys
vecKeys = do
  rev <- pt
    "0212a140cd0c6539d07cd08dfe09984dec3251ea808b892efeac3ede9402bf2b19"
  delayed <- pt
    "03fd5960528dc152014952efdb702a88f71e3c1653b2314431701ec77e57fde83c"
  localHtlc <- pt
    "030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e7"
  remoteHtlc <- pt
    "0394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b"
  remotePayment <- pt
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  localFunding <- pt
    "023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"
  remoteFunding <- pt
    "030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c1"
  pure CommitmentKeys
    { ck_revocation_pubkey = RevocationPubkey rev
    , ck_local_delayed     = LocalDelayedPubkey delayed
    , ck_local_htlc        = LocalHtlcPubkey localHtlc
    , ck_remote_htlc       = RemoteHtlcPubkey remoteHtlc
    , ck_remote_payment    = RemotePubkey remotePayment
    , ck_local_funding     = FundingPubkey localFunding
    , ck_remote_funding    = FundingPubkey remoteFunding
    }

-- | Rebuild an Appendix C/F local commitment with bolt3, given the
--   feerate, commitment format, to_local balance (msat) and HTLCs.
vecCommitment
  :: FeeratePerKw -> CommitmentFormat -> Word64 -> [HTLC]
  -> IO CommitmentTx
vecCommitment feerate fmt toLocal htlcs = do
  keys <- vecKeys
  fundingTxid <- need "funding txid" $ BT.mk_txid =<< unhex
    "bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489"
  cn <- need "commitment number" (commitment_number 42)
  localBp <- pt
    "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
  remoteBp <- pt
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  dust <- sat 546
  toLocalMsat <- msat toLocal
  toRemoteMsat <- msat 3000000000
  need "build_commitment_tx" $ build_commitment_tx CommitmentContext
    { cc_funding_outpoint  = OutPoint fundingTxid 0
    , cc_commitment_number = cn
    , cc_local_payment_bp  = PaymentBasepoint localBp
    , cc_remote_payment_bp = PaymentBasepoint remoteBp
    , cc_to_self_delay     = ToSelfDelay 144
    , cc_dust_limit        = DustLimit dust
    , cc_feerate           = feerate
    , cc_format            = fmt
    , cc_is_funder         = True
    , cc_to_local_msat     = toLocalMsat
    , cc_to_remote_msat    = toRemoteMsat
    , cc_htlcs             = htlcs
    , cc_keys              = keys
    }

-- | The Appendix C commitment with two offered HTLCs sharing a
--   payment hash and amount.
vecCommitSameHash :: IO CommitmentTx
vecCommitSameHash = do
  h1 <- vecHtlc1
  h5 <- vecHtlc5
  h6 <- vecHtlc6
  vecCommitment (FeeratePerKw 253) StaticRemotekey 6987999999 [h1, h5, h6]

-- | The Appendix F (anchors) "commitment tx with seven outputs
--   untrimmed".
vecCommitAnchors :: IO CommitmentTx
vecCommitAnchors = do
  hs <- sequence [vecHtlc0, vecHtlc1, vecHtlc2, vecHtlc3, vecHtlc4]
  vecCommitment (FeeratePerKw 644) Anchors 6988000000 hs

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

-- | Appendix C: HTLC-timeout tx for htlc #6 from the same
--   commitment (signed).
vecCHtlcTimeout6 :: BS.ByteString
vecCHtlcTimeout6 =
  "020000000001014bdccf28653066a2c554cafeffdfe1e678e64a69b056684deb\
  \0c4fba909423ec01000000000000000001e1120000000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004830\
  \45022100b575379f6d8743cb0087648f81cfd82d17a97fbf8f67e058c65ce8b9\
  \d25df9500220554a210d65b02d9f36c6adf0f639430ca8293196ba5089bf67cc\
  \3a9813b7b00a01483045022100ee2e16b90930a479b13f8823a7f14b600198c8\
  \38161160b9436ed086d3fc57e002202a66fa2324f342a17129949c640bfe934c\
  \bc73a869ba7c06aa25c5a3d0bfb53d01008576a91414011f7254d96b819c7698\
  \6c277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a\
  \2184d88256816826d6231c068d4a5b7c820120876475527c21030d417a469463\
  \84f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a91420\
  \02cc93ebefbb1b73f0af055dcc27a0b504ad7688ac6868f9010000"

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

-- | Appendix F: HTLC-timeout tx for htlc #2 from "commitment tx
--   with seven outputs untrimmed" (signed).
vecFHtlcTimeout2 :: BS.ByteString
vecFHtlcTimeout2 =
  "02000000000101b8cefef62ea66f5178b9361b2371be0759cbc8c689bcfa7a8e\
  \6746d497ec221a03000000000100000001d0070000000000002200204adb4e2f\
  \00643db396dd120d4e7dc17625f5f2c11a40d857accc862d6b7dd80e05004830\
  \45022100a847bc3b8cf2441013725ae32dc589c419835d069afd6d7f3d9834d8\
  \be7cea6e02204ce9d35ec7f0788da80e4d62e810c4ccd13617e58fa10832e38f\
  \b7ae87bdf33883473044022026739b1adbfa34c485bf0e5a19e0cf7532f64545\
  \bcc5c95b95643ccc2d351e1902201743bb1b34f00e031cc15f6eff0f8ab76450\
  \8d2681ff965ba62e76e540bca1e801008876a91414011f7254d96b819c76986c\
  \277d115efce6f7b58763ac67210394854aa6eab5b2a8122cc726e9dded053a21\
  \84d88256816826d6231c068d4a5b7c820120876475527c21030d417a46946384\
  \f88d5f3337267c5e579765875dc4daca813e21734b140639e752ae67a914b43e\
  \1b38138a41b37f7cd9a1d274bc63e3a9b5d188ac6851b27568f6010000"

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
parseTx = need "invalid transaction hex" . BT.from_base16

-- | Witness stack of a single-input transaction.
singleWitness :: Tx -> IO [BS.ByteString]
singleWitness tx = case tx_inputs tx of
  i :| [] | Witness items <- txin_witness i -> pure items
  _ -> assertFailure "expected a single input"

-- | Remote signature and offered HTLC witness script, taken from the
--   Appendix C HTLC-timeout tx for htlc #5.
offeredClaimParts :: IO (BS.ByteString, Script)
offeredClaimParts = do
  items <- singleWitness =<< parseTx vecCHtlcTimeout5
  case items of
    [_, sig, _, _, script] -> pure (sig, Script script)
    _ -> assertFailure "unexpected HTLC-timeout witness"

-- | Signatures and received HTLC witness script, taken from the
--   Appendix C HTLC-success tx for htlc #1.
successParts :: IO (BS.ByteString, BS.ByteString, Script)
successParts = do
  items <- singleWitness =<< parseTx vecCHtlcSuccess1
  case items of
    [_, remoteSig, localSig, _, script] ->
      pure (remoteSig, localSig, Script script)
    _ -> assertFailure "unexpected HTLC-success witness"

-- | Build the HTLC tx for an HTLC output of an Appendix C/F
--   commitment (to_self_delay 144).
vecHtlcTx :: FeeratePerKw -> B5.UnresolvedOutput -> IO B5.SpendingTx
vecHtlcTx feerate uo = case B5.uo_type uo of
  B5.SpendHTLCTimeout htlc keys fmt -> build htlc keys fmt
  B5.SpendHTLCSuccess htlc keys fmt -> build htlc keys fmt
  other -> assertFailure ("expected an HTLC output, got " <> show other)
  where
    build htlc keys fmt = need "spend_htlc_tx" $
      B5.spend_htlc_tx (ctx htlc keys fmt) keys
    ctx htlc keys fmt = HTLCContext
      { hc_commitment_txid   = op_txid (B5.uo_outpoint uo)
      , hc_output_index      = op_vout (B5.uo_outpoint uo)
      , hc_htlc              = htlc
      , hc_to_self_delay     = ToSelfDelay 144
      , hc_feerate           = feerate
      , hc_format            = fmt
      , hc_revocation_pubkey = ck_revocation_pubkey keys
      , hc_local_delayed     = ck_local_delayed keys
      }

-- | Check an HTLC tx against a signed BOLT #3 vector: the unsigned
--   tx and the witness script must match.
assertHtlcTx :: B5.SpendingTx -> BS.ByteString -> Assertion
assertHtlcTx stx vec = do
  tx <- parseTx vec
  items <- singleWitness tx
  B5.stx_tx stx @?= tx { tx_inputs = fmap
    (\i -> i { txin_witness = Witness [] }) (tx_inputs tx) }
  case reverse items of
    script : _ -> B5.stx_input_script stx @?= Script script
    [] -> assertFailure "empty witness"

-- | Check the sighash flags of the signatures in a signed BOLT #3
--   HTLC-tx vector: the local one must be the type the 'SpendingTx'
--   gives, the remote one bolt3's 'remote_htlc_sighash'.
assertSighashFlags
  :: B5.SpendingTx -> CommitmentFormat -> BS.ByteString -> Assertion
assertSighashFlags stx fmt vec = do
  items <- singleWitness =<< parseTx vec
  case items of
    [_, remoteSig, localSig, _, _] -> do
      flag remoteSig @?= Just (encode_sighash (remote_htlc_sighash fmt))
      flag localSig
        @?= Just (encode_sighash (B5.stx_sighash_type stx))
    _ -> assertFailure "unexpected HTLC-tx witness"
  where
    flag = fmap (fromIntegral . snd) . BS.unsnoc

-- types tests --------------------------------------------------------

types_tests :: TestTree
types_tests = testGroup "Types" [
    testCase "CloseType constructors" $ do
      B5.MutualClose @?= B5.MutualClose
      B5.LocalCommitClose @?= B5.LocalCommitClose
      B5.RemoteCommitClose @?= B5.RemoteCommitClose
      B5.RevokedCommitClose @?= B5.RevokedCommitClose

  , testCase "spending_fee calculation" $
      BOLT1.un_satoshi (B5.spending_fee (FeeratePerKw 1000) 500) @?= 500

  , testCase "spending_fee at low feerate" $
      -- 253 * 324 / 1000 = 81.972 -> 81
      BOLT1.un_satoshi (B5.spending_fee (FeeratePerKw 253) 324) @?= 81

  , testCase "spending_fee saturates at 21 million BTC" $
      B5.spending_fee (FeeratePerKw maxBound) maxBound
        @?= BOLT1.max_satoshi

  , testCase "weight constants" $ do
      B5.to_local_penalty_witness_weight @?= 160
      B5.to_local_penalty_input_weight @?= 324
      B5.offered_htlc_penalty_input_weight @?= 407
      B5.accepted_htlc_penalty_input_weight @?= 413
      B5.offered_htlc_penalty_input_weight_anchors @?= 410
      B5.accepted_htlc_penalty_input_weight_anchors @?= 416
      B5.to_remote_input_weight @?= 272
      B5.max_standard_weight @?= 400000
  ]

-- detect tests -------------------------------------------------------

detect_tests :: TestTree
detect_tests = testGroup "Detect" [
    testCase "extract_preimage_htlc_success - Appendix C" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      ph <- vecPaymentHash 0x01
      fmap BOLT1.un_payment_preimage
        (B5.extract_preimage_htlc_success ph (Witness items))
        @?= Just (BS.replicate 32 0x01)

  , testCase "extract_preimage_htlc_success - Appendix F" $ do
      items <- singleWitness =<< parseTx vecFHtlcSuccess1
      ph <- vecPaymentHash 0x01
      fmap BOLT1.un_payment_preimage
        (B5.extract_preimage_htlc_success ph (Witness items))
        @?= Just (BS.replicate 32 0x01)

  , testCase "extract_preimage_htlc_success - bolt3 witness" $ do
      (remoteSig, localSig, script) <- successParts
      ph <- vecPaymentHash 0x01
      preimage <- need "preimage"
        (BOLT1.payment_preimage (BS.replicate 32 0x01))
      B5.extract_preimage_htlc_success ph
        (htlc_success_witness remoteSig localSig preimage script)
        @?= Just preimage

  , testCase "extract_preimage_htlc_success - wrong hash" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      ph <- vecPaymentHash 0x05
      B5.extract_preimage_htlc_success ph (Witness items) @?= Nothing

  , testCase "extract_preimage_htlc_success - timeout witness" $ do
      items <- singleWitness =<< parseTx vecCHtlcTimeout5
      ph <- vecPaymentHash 0x05
      B5.extract_preimage_htlc_success ph (Witness items) @?= Nothing
      (remoteSig, localSig, script) <- successParts
      B5.extract_preimage_htlc_success ph
        (htlc_timeout_witness remoteSig localSig script)
        @?= Nothing

  , testCase "extract_preimage_htlc_success - no script" $ do
      items <- singleWitness =<< parseTx vecCHtlcSuccess1
      ph <- vecPaymentHash 0x01
      B5.extract_preimage_htlc_success ph (Witness (take 4 items))
        @?= Nothing

  , testCase "extract_preimage_offered - remote claim" $ do
      -- <remotehtlcsig> <payment_preimage> <witness_script>; the
      -- signature is not checked.
      (sig, script) <- offeredClaimParts
      ph <- vecPaymentHash 0x05
      preimage <- need "preimage"
        (BOLT1.payment_preimage (BS.replicate 32 0x05))
      B5.extract_preimage_offered ph
        (offered_htlc_witness_preimage sig preimage script)
        @?= Just preimage

  , testCase "extract_preimage_offered - wrong hash" $ do
      (sig, script) <- offeredClaimParts
      ph <- vecPaymentHash 0x01
      preimage <- need "preimage"
        (BOLT1.payment_preimage (BS.replicate 32 0x05))
      B5.extract_preimage_offered ph
        (offered_htlc_witness_preimage sig preimage script)
        @?= Nothing

  , testCase "extract_preimage_offered - revocation spend" $ with_fx $
      \fx -> do
        -- <revocation_sig> <revocationpubkey> <witness_script>
        (sig, script) <- offeredClaimParts
        ph <- vecPaymentHash 0x05
        B5.extract_preimage_offered ph
          (offered_htlc_witness_revoke sig (fx_revpk fx) script)
          @?= Nothing

  , testCase "extract_preimage_offered - no script" $ do
      (sig, _) <- offeredClaimParts
      ph <- vecPaymentHash 0x05
      B5.extract_preimage_offered ph
        (Witness [sig, BS.replicate 32 0x05])
        @?= Nothing

  , testCase "htlc_timed_out - at expiry" $ with_fx $ \fx ->
      B5.htlc_timed_out 500000 (fx_offered fx) @?= True

  , testCase "htlc_timed_out - past expiry" $ with_fx $ \fx ->
      B5.htlc_timed_out 500001 (fx_offered fx) @?= True

  , testCase "htlc_timed_out - before expiry" $ with_fx $ \fx ->
      B5.htlc_timed_out 499999 (fx_offered fx) @?= False
  ]

-- classify tests -----------------------------------------------------

classify_tests :: TestTree
classify_tests = testGroup "Classify" [
    testCase "identify_close - signed local commitment" $ with_fx $
      \fx -> do
        commit <- vecCommitSameHash
        signed <- need "hex" (unhex vecCCommitSameHash)
        B5.identify_close commit (fx_remote_commit fx) signed
          @?= Just B5.LocalCommitClose

  , testCase "identify_close - signed remote commitment" $ with_fx $
      \fx -> do
        commit <- vecCommitSameHash
        signed <- need "hex" (unhex vecCCommitSameHash)
        B5.identify_close (fx_local_commit fx) commit signed
          @?= Just B5.RemoteCommitClose

  , testCase "identify_close - unsigned commitment" $ with_fx $ \fx -> do
      commit <- vecCommitSameHash
      B5.identify_close commit (fx_remote_commit fx)
        (encode_commitment_tx commit)
        @?= Just B5.LocalCommitClose

  , testCase "identify_close - no match" $ with_fx $ \fx -> do
      signed <- need "hex" (unhex vecCCommitSameHash)
      B5.identify_close (fx_local_commit fx) (fx_remote_commit fx) signed
        @?= Nothing

  , testCase "identify_close - unparseable" $ with_fx $ \fx ->
      B5.identify_close (fx_local_commit fx) (fx_remote_commit fx)
        "unknown bytes"
        @?= Nothing

  , testCase "classify_local - to_local and to_remote" $ with_fx $ \fx ->
      map B5.uo_type
        (B5.classify_local_commit_outputs (fx_local_commit fx)
           (fx_keys fx) dummyDelay StaticRemotekey)
        @?= [ B5.SpendToLocal dummyDelay (fx_revpk fx) (fx_delayed fx)
            , B5.Resolved
            ]

  , testCase "classify_local - outpoints and values" $ with_fx $ \fx -> do
      let commit = fx_htlc_commit fx
          txid = BT.txid (commitment_to_tx commit)
          outs = B5.classify_local_commit_outputs commit (fx_keys fx)
            dummyDelay StaticRemotekey
      map B5.uo_outpoint outs @?= map (OutPoint txid) [0 .. 3]
      map (BOLT1.un_satoshi . B5.uo_value) outs
        @?= [50000, 30000, 1000, 1000]

  , testCase "classify_local - HTLC outputs" $ with_fx $ \fx ->
      map B5.uo_type
        (drop 2 (B5.classify_local_commit_outputs (fx_htlc_commit fx)
           (fx_keys fx) dummyDelay StaticRemotekey))
        @?= [ B5.SpendHTLCTimeout (fx_offered fx) (fx_keys fx)
                StaticRemotekey
            , B5.SpendHTLCSuccess (fx_received fx) (fx_keys fx)
                StaticRemotekey
            ]

  , testCase "classify_remote - HTLC outputs" $ with_fx $ \fx ->
      map B5.uo_type
        (B5.classify_remote_commit_outputs (fx_htlc_commit fx)
           (fx_keys fx))
        @?= [ B5.Resolved
            , B5.Resolved
            , B5.SpendHTLCPreimageDirect (fx_offered fx)
            , B5.SpendHTLCTimeoutDirect (fx_received fx)
            ]

  , testCase "classify_revoked - to_local and HTLCs" $ with_fx $ \fx ->
      map B5.uo_type
        (B5.classify_revoked_commit_outputs (fx_htlc_commit fx)
           (fx_revpk fx))
        @?= [ B5.Revoke (fx_revpk fx)
            , B5.Resolved
            , B5.RevokeHTLC (fx_revpk fx)
                (B5.HTLCOfferedOutput (CltvExpiry 500000))
            , B5.RevokeHTLC (fx_revpk fx)
                (B5.HTLCReceivedOutput (CltvExpiry 500000))
            ]

  , testCase "classify - anchor outputs" $ with_fx $ \fx -> do
      v330 <- sat 330
      let keys = fx_keys fx
          anchor pk = to_p2wsh (anchor_script pk)
          commit = (fx_local_commit fx)
            { ctx_outputs =
                   CommitmentOutput v330 (anchor (ck_local_funding keys))
                     OutputLocalAnchor
                :| [CommitmentOutput v330
                      (anchor (ck_remote_funding keys))
                      OutputRemoteAnchor]
            }
      map B5.uo_type
        (B5.classify_local_commit_outputs commit keys dummyDelay Anchors)
        @?= [B5.AnchorSpend (ck_local_funding keys), B5.Resolved]
      map B5.uo_type (B5.classify_remote_commit_outputs commit keys)
        @?= [B5.Resolved, B5.AnchorSpend (ck_remote_funding keys)]
      map B5.uo_type
        (B5.classify_revoked_commit_outputs commit (fx_revpk fx))
        @?= [B5.Resolved, B5.Resolved]

  , testCase "classify_remote - non-HTLC outputs" $ with_fx $ \fx ->
      map B5.uo_type
        (B5.classify_remote_commit_outputs (fx_remote_commit fx)
           (fx_keys fx))
        @?= [B5.Resolved, B5.Resolved]

  , testCase "classify_local - HTLCs sharing a payment hash" $ do
      -- Appendix C: htlcs #5 and #6 share a payment hash and amount
      -- (5000 sat) and differ only in CLTV expiry (506 and 505).
      commit <- vecCommitSameHash
      keys <- vecKeys
      h1 <- vecHtlc1
      h5 <- vecHtlc5
      h6 <- vecHtlc6
      let outs = B5.classify_local_commit_outputs commit keys
            (ToSelfDelay 144) StaticRemotekey
      map B5.uo_type (take 3 outs) @?=
        [ B5.SpendHTLCSuccess h1 keys StaticRemotekey
        , B5.SpendHTLCTimeout h6 keys StaticRemotekey
        , B5.SpendHTLCTimeout h5 keys StaticRemotekey
        ]

  , testCase "HTLC txs - Appendix C, shared payment hash" $ do
      commit <- vecCommitSameHash
      keys <- vecKeys
      let outs = B5.classify_local_commit_outputs commit keys
            (ToSelfDelay 144) StaticRemotekey
      case outs of
        o0 : o1 : o2 : _ -> do
          stx0 <- vecHtlcTx (FeeratePerKw 253) o0
          assertHtlcTx stx0 vecCHtlcSuccess1
          stx1 <- vecHtlcTx (FeeratePerKw 253) o1
          assertHtlcTx stx1 vecCHtlcTimeout6
          assertSighashFlags stx1 StaticRemotekey vecCHtlcTimeout6
          stx2 <- vecHtlcTx (FeeratePerKw 253) o2
          assertHtlcTx stx2 vecCHtlcTimeout5
        _ -> assertFailure "expected at least 3 outputs"

  , testCase "classify_remote - HTLCs sharing a payment hash" $ do
      commit <- vecCommitSameHash
      keys <- vecKeys
      h1 <- vecHtlc1
      h5 <- vecHtlc5
      h6 <- vecHtlc6
      let outs = B5.classify_remote_commit_outputs commit keys
      map B5.uo_type (take 3 outs) @?=
        [ B5.SpendHTLCTimeoutDirect h1
        , B5.SpendHTLCPreimageDirect h6
        , B5.SpendHTLCPreimageDirect h5
        ]

  , testCase "classify_local - HTLCs differing only in amount" $ do
      hA <- vecHtlc HTLCOffered 5000000 600 0x07
      hB <- vecHtlc HTLCOffered 9000000 600 0x07
      commit <- vecCommitment (FeeratePerKw 253) StaticRemotekey
        6000000000 [hB, hA]
      keys <- vecKeys
      let outs = B5.classify_local_commit_outputs commit keys
            (ToSelfDelay 144) StaticRemotekey
      [ (BOLT1.un_satoshi (B5.uo_value o), h)
        | o <- outs
        , B5.SpendHTLCTimeout h _ _ <- [B5.uo_type o] ]
        @?= [(5000, hA), (9000, hB)]

  , testCase "classify_local - one resolution per HTLC output" $ do
      h <- vecHtlc HTLCOffered 5000000 600 0x07
      keys <- vecKeys
      let timeouts commit = length
            [ ()
            | o <- B5.classify_local_commit_outputs commit keys
                     (ToSelfDelay 144) StaticRemotekey
            , B5.SpendHTLCTimeout {} <- [B5.uo_type o] ]
      one <- vecCommitment (FeeratePerKw 253) StaticRemotekey
        6000000000 [h]
      two <- vecCommitment (FeeratePerKw 253) StaticRemotekey
        6000000000 [h, h]
      timeouts one @?= 1
      timeouts two @?= 2
  ]

-- spend tests --------------------------------------------------------

spend_tests :: TestTree
spend_tests = testGroup "Spend" [
    testCase "spend_to_local produces valid tx" $ with_fx $ \fx -> do
      value <- sat 100000
      stx <- need "spend_to_local" $
        B5.spend_to_local dummyOutPoint value (fx_revpk fx) dummyDelay
          (fx_delayed fx) dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      length (tx_outputs tx) @?= 1
      assertBool "output < input" (out_value stx < 100000)
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx @?= value
      B5.stx_input_script stx
        @?= to_local_script (fx_revpk fx) dummyDelay (fx_delayed fx)
      -- nSequence encodes the delay
      in_sequence stx @?= 144

  , testCase "spend_to_local fee deduction" $ with_fx $ \fx -> do
      value <- sat 100000
      stx <- need "spend_to_local" $
        B5.spend_to_local dummyOutPoint value (fx_revpk fx) dummyDelay
          (fx_delayed fx) dummyDestScript dummyFeerate
      let fee = B5.spending_fee dummyFeerate
            (B5.to_local_penalty_input_weight + B5.penalty_tx_base_weight)
      out_value stx @?= 100000 - BOLT1.un_satoshi fee

  , testCase "spend_revoked_to_local nSequence" $ with_fx $ \fx -> do
      value <- sat 100000
      stx <- need "spend_revoked_to_local" $
        B5.spend_revoked_to_local dummyOutPoint value (fx_revpk fx)
          dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
      in_sequence stx @?= 0xFFFFFFFF
      B5.stx_input_script stx
        @?= to_local_script (fx_revpk fx) dummyDelay (fx_delayed fx)

  , testCase "spend_anchor_owner tx structure" $ with_fx $ \fx -> do
      value <- sat 330
      let stx = B5.spend_anchor_owner dummyOutPoint value
            (fx_funding fx) dummyDestScript
          tx = B5.stx_tx stx
      tx_version tx @?= 2
      tx_locktime tx @?= 0
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_script stx @?= anchor_script (fx_funding fx)

  , testCase "spend_anchor_anyone nSequence" $ with_fx $ \fx -> do
      value <- sat 330
      let stx = B5.spend_anchor_anyone dummyOutPoint value
            (fx_funding fx) dummyDestScript
      in_sequence stx @?= 16

  , testCase "spend_htlc_output is spend_to_local" $ with_fx $ \fx -> do
      value <- sat 50000
      let spend f = f dummyOutPoint value (fx_revpk fx) dummyDelay
            (fx_delayed fx) dummyDestScript dummyFeerate
      stx1 <- need "spend_to_local" (spend B5.spend_to_local)
      stx2 <- need "spend_htlc_output" (spend B5.spend_htlc_output)
      stx1 @?= stx2

  , testCase "spend_to_local fee underflow" $ with_fx $ \fx -> do
      value <- sat 1
      B5.spend_to_local dummyOutPoint value (fx_revpk fx) dummyDelay
        (fx_delayed fx) dummyDestScript dummyFeerate
        @?= Nothing

  , testCase "spend_to_local dust threshold by script" $ with_fx $ \fx -> do
      let fee = BOLT1.un_satoshi $ B5.spending_fee dummyFeerate
            (B5.to_local_penalty_input_weight + B5.penalty_tx_base_weight)
          sweep dest v = do
            value <- sat v
            pure $ B5.spend_to_local dummyOutPoint value (fx_revpk fx)
              dummyDelay (fx_delayed fx) dest dummyFeerate
          check (name, dest, dust) = do
            at <- sweep dest (fee + dust)
            assertEqual (name <> " at threshold") (Just dust)
              (fmap out_value at)
            below <- sweep dest (fee + dust - 1)
            assertEqual (name <> " below threshold") Nothing below
      mapM_ check
        [ ("p2pkh", Script $ BS.pack ([0x76, 0xa9, 0x14]
            <> replicate 20 0xCC <> [0x88, 0xac]), 546)
        , ("p2sh", Script $ BS.pack ([0xa9, 0x14]
            <> replicate 20 0xCC <> [0x87]), 540)
        , ("p2wpkh", dummyDestScript, 294)
        , ("p2wsh", Script $ BS.pack ([0x00, 0x20]
            <> replicate 32 0xCC), 330)
        , ("p2tr", Script $ BS.pack ([0x51, 0x20]
            <> replicate 32 0xCC), 330)
        ]
  ]

-- htlc spend tests ---------------------------------------------------

htlc_spend_tests :: TestTree
htlc_spend_tests = testGroup "HTLC Spend" [
    testCase "spend_htlc_tx - HTLC-timeout" $ with_fx $ \fx -> do
      stx <- need "spend_htlc_tx" $ B5.spend_htlc_tx
        (htlc_context fx (fx_offered fx) StaticRemotekey) (fx_keys fx)
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      length (tx_outputs tx) @?= 1
      tx_locktime tx @?= 500000
      in_sequence stx @?= 0
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx
        @?= BOLT1.msat_to_sat (htlc_amount_msat (fx_offered fx))
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_offered fx)

  , testCase "spend_htlc_tx - HTLC-success" $ with_fx $ \fx -> do
      stx <- need "spend_htlc_tx" $ B5.spend_htlc_tx
        (htlc_context fx (fx_received fx) StaticRemotekey) (fx_keys fx)
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      tx_locktime tx @?= 0
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_received fx)

  , testCase "spend_htlc_tx - HTLC-timeout anchors sighash" $ with_fx $
      \fx -> do
        stx <- need "spend_htlc_tx" $ B5.spend_htlc_tx
          (htlc_context fx (fx_offered fx) Anchors) (fx_keys fx)
        B5.stx_sighash_type stx @?= SIGHASH_ALL
        in_sequence stx @?= 1

  , testCase "spend_htlc_tx - HTLC-success anchors sighash" $ with_fx $
      \fx -> do
        stx <- need "spend_htlc_tx" $ B5.spend_htlc_tx
          (htlc_context fx (fx_received fx) Anchors) (fx_keys fx)
        B5.stx_sighash_type stx @?= SIGHASH_ALL

  , testCase "spend_htlc_tx - fee exceeds amount" $ with_fx $ \fx -> do
      -- 5000 * 663 / 1000 = 3315 sat, more than the 1000 sat HTLC
      let ctx = (htlc_context fx (fx_offered fx) StaticRemotekey)
            { hc_feerate = FeeratePerKw 5000 }
      B5.spend_htlc_tx ctx (fx_keys fx) @?= Nothing

  , testCase "HTLC txs - Appendix F (anchors)" $ do
      -- The local signature uses SIGHASH_ALL and the remote one
      -- SIGHASH_SINGLE|SIGHASH_ANYONECANPAY.
      commit <- vecCommitAnchors
      keys <- vecKeys
      let outs = B5.classify_local_commit_outputs commit keys
            (ToSelfDelay 144) Anchors
          check uo vec = do
            stx <- vecHtlcTx (FeeratePerKw 644) uo
            assertHtlcTx stx vec
            B5.stx_sighash_type stx @?= SIGHASH_ALL
            assertSighashFlags stx Anchors vec
      case drop 3 outs of
        o3 : o4 : _ -> do
          check o3 vecFHtlcTimeout2
          check o4 vecFHtlcSuccess1
        _ -> assertFailure "expected at least 5 outputs"
  ]

-- remote spend tests -------------------------------------------------

remote_spend_tests :: TestTree
remote_spend_tests = testGroup "Remote Spend" [
    testCase "spend_remote_htlc_timeout structure" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout dummyOutPoint value (fx_offered fx)
          (fx_keys fx) StaticRemotekey dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      length (tx_inputs tx) @?= 1
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_value stx @?= value
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_received fx)
      -- locktime is the HTLC's CLTV expiry
      tx_locktime tx @?= 500000

  , testCase "spend_remote_htlc_timeout fee deduction" $ with_fx $
      \fx -> do
        value <- sat 50000
        stx <- need "spend_remote_htlc_timeout" $
          B5.spend_remote_htlc_timeout dummyOutPoint value
            (fx_offered fx) (fx_keys fx) StaticRemotekey
            dummyDestScript dummyFeerate
        let fee = B5.spending_fee dummyFeerate
              (B5.accepted_htlc_penalty_input_weight
               + B5.penalty_tx_base_weight)
        out_value stx @?= 50000 - BOLT1.un_satoshi fee

  , testCase "spend_remote_htlc_preimage structure" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_remote_htlc_preimage" $
        B5.spend_remote_htlc_preimage dummyOutPoint value
          (fx_received fx) (fx_keys fx) StaticRemotekey
          dummyDestScript dummyFeerate
      let tx = B5.stx_tx stx
      tx_version tx @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_offered fx)
      -- locktime is 0 for preimage claims
      tx_locktime tx @?= 0

  , testCase "spend_remote_htlc_timeout anchors seq" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_remote_htlc_timeout" $
        B5.spend_remote_htlc_timeout dummyOutPoint value (fx_offered fx)
          (fx_keys fx) Anchors dummyDestScript dummyFeerate
      in_sequence stx @?= 1

  , testCase "spend_remote_htlc_timeout no-anchor seq" $ with_fx $
      \fx -> do
        value <- sat 50000
        stx <- need "spend_remote_htlc_timeout" $
          B5.spend_remote_htlc_timeout dummyOutPoint value
            (fx_offered fx) (fx_keys fx) StaticRemotekey
            dummyDestScript dummyFeerate
        in_sequence stx @?= 0

  , testCase "spend_remote_htlc_preimage nSequence" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_remote_htlc_preimage" $
        B5.spend_remote_htlc_preimage dummyOutPoint value
          (fx_received fx) (fx_keys fx) StaticRemotekey
          dummyDestScript dummyFeerate
      in_sequence stx @?= 0
  ]

-- revoked htlc spend tests ------------------------------------------

revoked_htlc_spend_tests :: TestTree
revoked_htlc_spend_tests = testGroup "Revoked HTLC Spend" [
    testCase "spend_revoked_htlc - offered" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_revoked_htlc" $
        B5.spend_revoked_htlc dummyOutPoint value
          (B5.HTLCOfferedOutput (CltvExpiry 500000)) (fx_revpk fx)
          (fx_keys fx) StaticRemotekey (fx_hash fx) dummyDestScript
          dummyFeerate
      tx_version (B5.stx_tx stx) @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      in_sequence stx @?= 0xFFFFFFFF
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_offered fx)

  , testCase "spend_revoked_htlc - received" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_revoked_htlc" $
        B5.spend_revoked_htlc dummyOutPoint value
          (B5.HTLCReceivedOutput (CltvExpiry 500000)) (fx_revpk fx)
          (fx_keys fx) StaticRemotekey (fx_hash fx) dummyDestScript
          dummyFeerate
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      assertBool "output < input" (out_value stx < 50000)
      B5.stx_input_script stx @?=
        htlc_witness_script (fx_keys fx) StaticRemotekey (fx_received fx)

  , testCase "htlc_output_type - HTLC outputs" $ with_fx $ \fx -> do
      B5.htlc_output_type (OutputHTLC (fx_offered fx))
        @?= Just (B5.HTLCOfferedOutput (CltvExpiry 500000))
      B5.htlc_output_type (OutputHTLC (fx_received fx))
        @?= Just (B5.HTLCReceivedOutput (CltvExpiry 500000))

  , testCase "htlc_output_type - non-HTLC outputs" $ do
      B5.htlc_output_type OutputToLocal @?= Nothing
      B5.htlc_output_type OutputToRemote @?= Nothing
      B5.htlc_output_type OutputLocalAnchor @?= Nothing
      B5.htlc_output_type OutputRemoteAnchor @?= Nothing

  , testCase "spend_revoked_htlc_output structure" $ with_fx $ \fx -> do
      value <- sat 50000
      stx <- need "spend_revoked_htlc_output" $
        B5.spend_revoked_htlc_output dummyOutPoint value (fx_revpk fx)
          dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
      tx_version (B5.stx_tx stx) @?= 2
      B5.stx_sighash_type stx @?= SIGHASH_ALL
      in_sequence stx @?= 0xFFFFFFFF
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
      -- with anchors: 164 + 246 = 410, 164 + 252 = 416
      B5.offered_htlc_penalty_input_weight_anchors @?=
        (164 + B5.offered_htlc_penalty_witness_weight_anchors)
      B5.accepted_htlc_penalty_input_weight_anchors @?=
        (164 + B5.accepted_htlc_penalty_witness_weight_anchors)

  , testCase "penalty witness weights match HTLC scripts" $ with_fx $
      \fx -> do
        -- 1 (item count) + 1 + 73 (signature) + 1 + 33
        -- (revocationpubkey) + 1 + script, for a 3-byte CLTV expiry
        let witnessWeight fmt h =
              let Script s = htlc_witness_script (fx_keys fx) fmt h
              in  fromIntegral (110 + BS.length s)
        witnessWeight StaticRemotekey (fx_offered fx)
          @?= B5.offered_htlc_penalty_witness_weight
        witnessWeight StaticRemotekey (fx_received fx)
          @?= B5.accepted_htlc_penalty_witness_weight
        witnessWeight Anchors (fx_offered fx)
          @?= B5.offered_htlc_penalty_witness_weight_anchors
        witnessWeight Anchors (fx_received fx)
          @?= B5.accepted_htlc_penalty_witness_weight_anchors

  , testCase "spend_revoked_htlc weight by commitment format" $ with_fx $
      \fx -> do
        value <- sat 50000
        let outValue fmt htype = fmap out_value
              (B5.spend_revoked_htlc dummyOutPoint value htype
                (fx_revpk fx) (fx_keys fx) fmt (fx_hash fx)
                dummyDestScript dummyFeerate)
            expected inputWeight = Just $ 50000 - BOLT1.un_satoshi
              (B5.spending_fee dummyFeerate
                (inputWeight + B5.penalty_tx_base_weight))
            offered = B5.HTLCOfferedOutput (CltvExpiry 500000)
            received = B5.HTLCReceivedOutput (CltvExpiry 500000)
        outValue StaticRemotekey offered
          @?= expected B5.offered_htlc_penalty_input_weight
        outValue StaticRemotekey received
          @?= expected B5.accepted_htlc_penalty_input_weight
        outValue Anchors offered
          @?= expected B5.offered_htlc_penalty_input_weight_anchors
        outValue Anchors received
          @?= expected B5.accepted_htlc_penalty_input_weight_anchors

  , testCase "max HTLCs in single penalty tx" $
      -- Per spec: (400000 - 324 - 272 - (4 * 53) - 2) / 413 = 966,
      -- where penalty_tx_base_weight = 4 * 53 + 2.
      (B5.max_standard_weight
        - B5.to_local_penalty_input_weight
        - B5.to_remote_input_weight
        - B5.penalty_tx_base_weight)
        `div` B5.accepted_htlc_penalty_input_weight
        @?= 966
  ]

-- property tests -----------------------------------------------------

property_tests :: TestTree
property_tests = testGroup "Properties" [
    testProperty "output + fee = input (spend_to_local)" $ prop_fx $
      \fx -> forAll (choose (100000, 2100000000000000)) $ \v ->
        let fee = B5.spending_fee dummyFeerate
              (B5.to_local_penalty_input_weight + B5.penalty_tx_base_weight)
            spent = do
              value <- BOLT1.satoshi v
              B5.spend_to_local dummyOutPoint value (fx_revpk fx)
                dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
        in  fmap (\stx -> out_value stx + BOLT1.un_satoshi fee) spent
              === Just v

  , testProperty "sweeps never create dust outputs" $ prop_fx $ \fx ->
      forAll (choose (0, 3000)) $ \v ->
        let sweeps value =
              [ B5.spend_to_local dummyOutPoint value (fx_revpk fx)
                  dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
              , B5.spend_remote_htlc_timeout dummyOutPoint value
                  (fx_offered fx) (fx_keys fx) StaticRemotekey
                  dummyDestScript dummyFeerate
              , B5.spend_remote_htlc_preimage dummyOutPoint value
                  (fx_received fx) (fx_keys fx) StaticRemotekey
                  dummyDestScript dummyFeerate
              , B5.spend_revoked_to_local dummyOutPoint value
                  (fx_revpk fx) dummyDelay (fx_delayed fx)
                  dummyDestScript dummyFeerate
              , B5.spend_revoked_htlc dummyOutPoint value
                  (B5.HTLCOfferedOutput (CltvExpiry 500000))
                  (fx_revpk fx) (fx_keys fx) StaticRemotekey
                  (fx_hash fx) dummyDestScript dummyFeerate
              , B5.spend_revoked_htlc dummyOutPoint value
                  (B5.HTLCReceivedOutput (CltvExpiry 500000))
                  (fx_revpk fx) (fx_keys fx) StaticRemotekey
                  (fx_hash fx) dummyDestScript dummyFeerate
              , B5.spend_revoked_htlc_output dummyOutPoint value
                  (fx_revpk fx) dummyDelay (fx_delayed fx)
                  dummyDestScript dummyFeerate
              ]
            notDust = maybe True (\stx -> out_value stx >= 294)
        in  case BOLT1.satoshi v of
              Nothing -> counterexample "invalid amount" False
              Just value -> property (all notDust (sweeps value))

  , testProperty "htlc_timed_out monotonic" $ prop_fx $
      \fx (NonNegative h1) (NonNegative h2) ->
        let htlc = (fx_offered fx) { htlc_cltv_expiry = CltvExpiry 1000 }
            lo = min h1 h2
            hi = max h1 h2
        in  not (B5.htlc_timed_out lo htlc) || B5.htlc_timed_out hi htlc

  , testProperty "fee monotonic in feerate" $
      \r1 r2 ->
        let w = B5.to_local_penalty_input_weight + B5.penalty_tx_base_weight
            fee r = B5.spending_fee (FeeratePerKw r) w
        in  fee (min r1 r2) <= fee (max r1 r2)

  , testProperty "spend_to_local always version 2" $ prop_fx $
      \fx -> forAll (choose (100000, 2100000000000000)) $ \v ->
        let spent = do
              value <- BOLT1.satoshi v
              B5.spend_to_local dummyOutPoint value (fx_revpk fx)
                dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
        in  fmap (tx_version . B5.stx_tx) spent === Just 2

  , testProperty "revoked nSequence is 0xFFFFFFFF" $ prop_fx $
      \fx -> forAll (choose (100000, 2100000000000000)) $ \v ->
        let spent = do
              value <- BOLT1.satoshi v
              B5.spend_revoked_to_local dummyOutPoint value (fx_revpk fx)
                dummyDelay (fx_delayed fx) dummyDestScript dummyFeerate
        in  fmap in_sequence spent === Just 0xFFFFFFFF

  , testProperty "htlc_output_type preserves direction and expiry" $
      prop_fx $ \fx expiry ->
        let ce = CltvExpiry expiry
            at h = OutputHTLC h { htlc_cltv_expiry = ce }
        in  B5.htlc_output_type (at (fx_offered fx))
              == Just (B5.HTLCOfferedOutput ce)
            && B5.htlc_output_type (at (fx_received fx))
              == Just (B5.HTLCReceivedOutput ce)

  , testProperty "revoked_output_weight by type" $
      \expiry ->
        let ce = CltvExpiry expiry
            ro t = case BOLT1.satoshi 1000 of
              Nothing -> Nothing
              Just v -> Just (B5.RevokedOutput dummyOutPoint v t)
            weight fmt t = fmap (B5.revoked_output_weight fmt) (ro t)
            offered = B5.RevokedHTLC (B5.HTLCOfferedOutput ce)
            received = B5.RevokedHTLC (B5.HTLCReceivedOutput ce)
        in  weight StaticRemotekey B5.RevokedToLocal
              == Just B5.to_local_penalty_input_weight
            && weight StaticRemotekey offered
              == Just B5.offered_htlc_penalty_input_weight
            && weight StaticRemotekey received
              == Just B5.accepted_htlc_penalty_input_weight
            && weight Anchors B5.RevokedToLocal
              == Just B5.to_local_penalty_input_weight
            && weight Anchors offered
              == Just B5.offered_htlc_penalty_input_weight_anchors
            && weight Anchors received
              == Just B5.accepted_htlc_penalty_input_weight_anchors
  ]
