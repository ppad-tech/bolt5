module Main where

import Fixtures
import Lightning.Protocol.BOLT3
import qualified Lightning.Protocol.BOLT5 as B5
import Weigh

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = do
  fx <- fixtures
  let keys = fx_keys fx
      revpk = ck_revocation_pubkey keys
      delayed = ck_local_delayed keys
  mainWith $ do
    wgroup "detect" $ do
      func "identify_close"
        (B5.identify_close (fx_other fx) (fx_commit fx)) (fx_raw fx)
      func "classify_local_commit_outputs"
        (\c -> B5.classify_local_commit_outputs c keys (fx_delay fx)
           StaticRemotekey)
        (fx_commit fx)
      func "classify_remote_commit_outputs"
        (\c -> B5.classify_remote_commit_outputs c keys) (fx_commit fx)
      func "classify_revoked_commit_outputs"
        (\c -> B5.classify_revoked_commit_outputs c revpk) (fx_commit fx)
      func "extract_preimage_offered"
        (B5.extract_preimage_offered (fx_hash fx)) (fx_claim fx)
      func "extract_preimage_htlc_success"
        (B5.extract_preimage_htlc_success (fx_hash fx)) (fx_success fx)
    wgroup "spend" $ do
      func "spend_to_local"
        (\v -> B5.spend_to_local (fx_outpoint fx) v revpk (fx_delay fx)
           delayed (fx_dest fx) (fx_feerate fx))
        (fx_value fx)
      func "spend_htlc_tx" (B5.spend_htlc_tx (fx_htlc_ctx fx)) keys
      func "spend_remote_htlc_timeout"
        (\v -> B5.spend_remote_htlc_timeout (fx_outpoint fx) v
           (fx_received fx) keys StaticRemotekey (fx_dest fx)
           (fx_feerate fx))
        (fx_value fx)
      func "spend_revoked_htlc"
        (\v -> B5.spend_revoked_htlc (fx_outpoint fx) v
           (B5.HTLCReceivedOutput (CltvExpiry 501)) revpk keys
           StaticRemotekey (fx_hash fx) (fx_dest fx) (fx_feerate fx))
        (fx_value fx)
      func "spend_anchor_owner"
        (\v -> B5.spend_anchor_owner (fx_outpoint fx) v
           (ck_local_funding keys) (fx_dest fx))
        (fx_anchor fx)
