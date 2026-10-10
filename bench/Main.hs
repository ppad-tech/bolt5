module Main where

import Criterion.Main
import Fixtures
import Lightning.Protocol.BOLT3
import qualified Lightning.Protocol.BOLT5 as B5

main :: IO ()
main = do
  fx <- fixtures
  let keys = fx_keys fx
      revpk = ck_revocation_pubkey keys
      delayed = ck_local_delayed keys
  defaultMain [
      bgroup "detect" [
        bench "identify_close" $
          nf (B5.identify_close (fx_other fx) (fx_commit fx)) (fx_raw fx)
      , bench "classify_local_commit_outputs" $
          nf (\c -> B5.classify_local_commit_outputs c keys (fx_delay fx)
                StaticRemotekey)
             (fx_commit fx)
      , bench "classify_remote_commit_outputs" $
          nf (\c -> B5.classify_remote_commit_outputs c keys) (fx_commit fx)
      , bench "classify_revoked_commit_outputs" $
          nf (\c -> B5.classify_revoked_commit_outputs c revpk)
             (fx_commit fx)
      , bench "extract_preimage_offered" $
          nf (B5.extract_preimage_offered (fx_hash fx)) (fx_claim fx)
      , bench "extract_preimage_htlc_success" $
          nf (B5.extract_preimage_htlc_success (fx_hash fx)) (fx_success fx)
      , bench "htlc_timed_out" $
          nf (B5.htlc_timed_out 500001) (fx_offered fx)
      ]
    , bgroup "spend" [
        bench "spend_to_local" $
          nf (\v -> B5.spend_to_local (fx_outpoint fx) v revpk (fx_delay fx)
                delayed (fx_dest fx) (fx_feerate fx))
             (fx_value fx)
      , bench "spend_htlc_tx" $
          nf (B5.spend_htlc_tx (fx_htlc_ctx fx)) keys
      , bench "spend_remote_htlc_timeout" $
          nf (\v -> B5.spend_remote_htlc_timeout (fx_outpoint fx) v
                (fx_received fx) keys StaticRemotekey (fx_dest fx)
                (fx_feerate fx))
             (fx_value fx)
      , bench "spend_remote_htlc_preimage" $
          nf (\v -> B5.spend_remote_htlc_preimage (fx_outpoint fx) v
                (fx_offered fx) keys StaticRemotekey (fx_dest fx)
                (fx_feerate fx))
             (fx_value fx)
      , bench "spend_revoked_to_local" $
          nf (\v -> B5.spend_revoked_to_local (fx_outpoint fx) v revpk
                (fx_delay fx) delayed (fx_dest fx) (fx_feerate fx))
             (fx_value fx)
      , bench "spend_revoked_htlc" $
          nf (\v -> B5.spend_revoked_htlc (fx_outpoint fx) v
                (B5.HTLCReceivedOutput (CltvExpiry 501)) revpk keys
                StaticRemotekey (fx_hash fx) (fx_dest fx) (fx_feerate fx))
             (fx_value fx)
      , bench "spend_anchor_owner" $
          nf (\v -> B5.spend_anchor_owner (fx_outpoint fx) v
                (ck_local_funding keys) (fx_dest fx))
             (fx_anchor fx)
      ]
    , bgroup "fees" [
        bench "spending_fee" $
          nf (B5.spending_fee (fx_feerate fx)) 538
      ]
    ]
