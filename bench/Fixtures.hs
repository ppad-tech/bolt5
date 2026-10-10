{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixtures (
    Fixtures(..)
  , fixtures
  ) where

import qualified Bitcoin.Prim.Tx as BT
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3

data Fixtures = Fixtures
  { fx_keys       :: !CommitmentKeys
  , fx_commit     :: !CommitmentTx
    -- ^ BOLT #3 Appendix C commitment with five untrimmed HTLCs
  , fx_other      :: !CommitmentTx
    -- ^ as 'fx_commit', with a different balance
  , fx_raw        :: !BS.ByteString
    -- ^ serialized 'fx_commit'
  , fx_outpoint   :: !BT.OutPoint
  , fx_value      :: !BOLT1.Satoshi
  , fx_anchor     :: !BOLT1.Satoshi
  , fx_dest       :: !Script
  , fx_feerate    :: !FeeratePerKw
  , fx_delay      :: !ToSelfDelay
  , fx_offered    :: !HTLC
  , fx_received   :: !HTLC
  , fx_htlc_ctx   :: !HTLCContext
    -- ^ for 'fx_offered'
  , fx_hash       :: !BOLT1.PaymentHash
    -- ^ payment hash of 'fx_offered' and 'fx_received'
  , fx_claim      :: !BT.Witness
    -- ^ remote preimage claim of an offered HTLC
  , fx_success    :: !BT.Witness
    -- ^ HTLC-success transaction witness
  } deriving Generic

instance NFData Fixtures

pt :: BS.ByteString -> Maybe BOLT1.Point
pt h = unhex h >>= BOLT1.point

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

-- | Benchmark fixtures, from the BOLT #3 Appendix C parameters, fully
--   evaluated.
fixtures :: IO Fixtures
fixtures = maybe (fail "invalid fixtures") (evaluate . force) build

build :: Maybe Fixtures
build = do
  rev <- pt
    "0212a140cd0c6539d07cd08dfe09984dec3251ea808b892efeac3ede9402bf2b19"
  delayed <- pt
    "03fd5960528dc152014952efdb702a88f71e3c1653b2314431701ec77e57fde83c"
  local_htlc <- pt
    "030d417a46946384f88d5f3337267c5e579765875dc4daca813e21734b140639e7"
  remote_htlc <- pt
    "0394854aa6eab5b2a8122cc726e9dded053a2184d88256816826d6231c068d4a5b"
  local_pay <- pt
    "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
  remote_pay <- pt
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  local_fund <- pt
    "023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"
  remote_fund <- pt
    "030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c1"
  funding_txid <- BT.mkTxId =<< unhex
    "bef67e4e2fb9ddeeb3461973cd4c62abb35050b1add772995b820b584a488489"
  cn <- commitment_number 42
  dust <- BOLT1.satoshi 546
  value <- BOLT1.satoshi 100000
  anchor <- BOLT1.satoshi 330
  to_remote <- BOLT1.milli_satoshi 3000000000
  h0 <- htlc HTLCReceived 1000000 500 0x00
  h1 <- htlc HTLCReceived 2000000 501 0x01
  h2 <- htlc HTLCOffered 2000000 502 0x02
  h3 <- htlc HTLCOffered 3000000 503 0x03
  h4 <- htlc HTLCReceived 4000000 504 0x04
  preimage <- BOLT1.payment_preimage (BS.replicate 32 0x01)
  let keys = CommitmentKeys
        { ck_revocation_pubkey = RevocationPubkey rev
        , ck_local_delayed     = LocalDelayedPubkey delayed
        , ck_local_htlc        = LocalHtlcPubkey local_htlc
        , ck_remote_htlc       = RemoteHtlcPubkey remote_htlc
        , ck_remote_payment    = RemotePubkey remote_pay
        , ck_local_funding     = FundingPubkey local_fund
        , ck_remote_funding    = FundingPubkey remote_fund
        }
      ctx to_local = do
        tl <- BOLT1.milli_satoshi to_local
        pure CommitmentContext
          { cc_funding_outpoint  = BT.OutPoint funding_txid 0
          , cc_commitment_number = cn
          , cc_local_payment_bp  = PaymentBasepoint local_pay
          , cc_remote_payment_bp = PaymentBasepoint remote_pay
          , cc_to_self_delay     = ToSelfDelay 144
          , cc_dust_limit        = DustLimit dust
          , cc_feerate           = FeeratePerKw 0
          , cc_format            = StaticRemotekey
          , cc_is_funder         = True
          , cc_to_local_msat     = tl
          , cc_to_remote_msat    = to_remote
          , cc_htlcs             = [h0, h1, h2, h3, h4]
          , cc_keys              = keys
          }
      sig = BS.replicate 71 0xCC
      script_of h = case htlc_direction h of
        HTLCOffered -> offered_htlc_script (RevocationPubkey rev)
          (RemoteHtlcPubkey remote_htlc) (LocalHtlcPubkey local_htlc)
          (htlc_payment_hash h) StaticRemotekey
        HTLCReceived -> received_htlc_script (RevocationPubkey rev)
          (RemoteHtlcPubkey remote_htlc) (LocalHtlcPubkey local_htlc)
          (htlc_payment_hash h) (htlc_cltv_expiry h) StaticRemotekey
      offered = h2 { htlc_payment_hash = htlc_payment_hash h1 }
  commit <- build_commitment_tx =<< ctx 6988000000
  other <- build_commitment_tx =<< ctx 6987000000
  pure Fixtures
    { fx_keys     = keys
    , fx_commit   = commit
    , fx_other    = other
    , fx_raw      = encode_commitment_tx commit
    , fx_outpoint = BT.OutPoint funding_txid 1
    , fx_value    = value
    , fx_anchor   = anchor
    , fx_dest     = Script (BS.pack [0x00, 0x14] <> BS.replicate 20 0xCC)
    , fx_feerate  = FeeratePerKw 253
    , fx_delay    = ToSelfDelay 144
    , fx_offered  = offered
    , fx_received = h1
    , fx_htlc_ctx = HTLCContext
        { hc_commitment_txid   = funding_txid
        , hc_output_index      = 2
        , hc_htlc              = offered
        , hc_to_self_delay     = ToSelfDelay 144
        , hc_feerate           = FeeratePerKw 253
        , hc_format            = StaticRemotekey
        , hc_revocation_pubkey = RevocationPubkey rev
        , hc_local_delayed     = LocalDelayedPubkey delayed
        }
    , fx_hash     = htlc_payment_hash h1
    , fx_claim    =
        offered_htlc_witness_preimage sig preimage (script_of offered)
    , fx_success  = htlc_success_witness sig sig preimage (script_of h1)
    }
  where
    htlc :: HTLCDirection -> Word64 -> Word32 -> Word8 -> Maybe HTLC
    htlc dir amt expiry b = do
      a <- BOLT1.milli_satoshi amt
      ph <- BOLT1.payment_hash (SHA256.hash (BS.replicate 32 b))
      pure (HTLC dir a ph (CltvExpiry expiry))
