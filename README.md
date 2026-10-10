# ppad-bolt5

[![](https://img.shields.io/hackage/v/ppad-bolt5?color=blue)](https://hackage.haskell.org/package/ppad-bolt5)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt5-lightblue)](https://docs.ppad.tech/bolt5)

A pure Haskell implementation of parts of [BOLT #5][bolt5], the
Lightning Network's rules for handling channel closes on chain.

## Status

**This library's scope is being redesigned, and it is not yet ready for
release.** Its API will change.

What it does:

* Identifies our local or the remote's current commitment on chain, by
  txid.
* Classifies the outputs of local, remote and revoked commitment
  transactions built by [ppad-bolt3][bolt3], reading each HTLC output's
  HTLC from bolt3.
* Extracts payment preimages from the witnesses of remote preimage
  claims and HTLC-success transactions, checking them against the
  payment hash.
* Builds unsigned, single-input spending transactions: to_local sweeps
  (delayed and revoked), HTLC-timeout and HTLC-success transactions
  (via bolt3), direct HTLC spends from the remote commitment, revoked
  HTLC sweeps, second-stage output sweeps and anchor spends. Sweeps
  refuse to create outputs below the destination's dust threshold.
* Provides the BOLT #5 Appendix A weights and fee calculation.

What it doesn't do (known gaps):

* Per-input signing data. A spending transaction describes one input
  and the local signature only; there is no batched penalty
  transaction.
* Witness assembly. The caller signs and builds every witness with
  bolt3's witness functions.
* Classification of raw on-chain transactions. The classifiers take the
  bolt3 commitment transaction for the state in question, which the
  caller must rebuild (often impossible for a revoked state).
* Revoked and mutual close detection. Only the current local and
  remote commitments are recognised, and the resolutions for a revoked
  commitment lack what's needed to build its spends (to_self_delay,
  delayed pubkey, payment hashes).
* Fee bumping. Anchor spends and anchor-channel HTLC transactions pay
  no fee and can't take fee inputs, and penalty sweeps don't signal
  replace-by-fee.
* `zero_fee_commitments` (version 3 transactions and the shared P2A
  anchor), and spending the anchor-channel to_remote output.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import qualified Bitcoin.Prim.Tx as BT
  > import qualified Crypto.Hash.SHA256 as SHA256
  > import qualified Data.ByteString as BS
  > import qualified Lightning.Protocol.BOLT1 as BOLT1
  > import Lightning.Protocol.BOLT3
  > import Lightning.Protocol.BOLT5
  >
  > -- fees from the BOLT #5 Appendix A weights
  > let feerate = FeeratePerKw 5000
  > let weight = to_local_penalty_input_weight + penalty_tx_base_weight
  > spending_fee feerate weight
  Satoshi 2690
  >
  > -- whether an HTLC has timed out at a given block height
  > let Just amt = BOLT1.milli_satoshi 50000000
  > let Just hash = BOLT1.payment_hash (BS.replicate 32 0xaa)
  > let htlc = HTLC HTLCOffered amt hash (CltvExpiry 800000)
  > htlc_timed_out 799999 htlc
  False
  > htlc_timed_out 800000 htlc
  True
  >
  > -- an unsigned sweep of our to_local output, valid after the delay
  > let key b = BOLT1.point (BS.cons 0x02 (BS.replicate 32 b))
  > let Just revpk = RevocationPubkey <$> key 0x01
  > let Just delayedpk = LocalDelayedPubkey <$> key 0x02
  > let Just txid = BT.mkTxId (BS.replicate 32 0x01)
  > let Just value = BOLT1.satoshi 1000000
  > let dest = Script ("\x00\x14" <> BS.replicate 20 0xcc)
  > let delay = ToSelfDelay 144
  > let op = BT.OutPoint txid 0
  > let Just stx = spend_to_local op value revpk delay delayedpk dest feerate
  > fmap BT.txout_value (BT.tx_outputs (stx_tx stx))
  997310 :| []
  > fmap BT.txin_sequence (BT.tx_inputs (stx_tx stx))
  144 :| []
  > stx_sighash_type stx
  SIGHASH_ALL
  > stx_input_script stx == to_local_script revpk delay delayedpk
  True
  >
  > -- extract the preimage from the remote's claim of an offered HTLC
  > let Just preimage = BOLT1.payment_preimage (BS.replicate 32 0xbb)
  > let Just ph = BOLT1.payment_hash (SHA256.hash (BS.replicate 32 0xbb))
  > let sig = BS.replicate 71 0xcc
  > let wit = offered_htlc_witness_preimage sig preimage (Script "")
  > extract_preimage_offered ph wit
  Just (PaymentPreimage <redacted>)
  > fmap (== preimage) (extract_preimage_offered ph wit)
  Just True
  > extract_preimage_offered hash wit
  Nothing
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt5](https://docs.ppad.tech/bolt5).

## Specification

This library tracks BOLT #5 as of lightning/bolts commit
[1aadb719][spec] (2026-09-21); a copy lives in `etc/`. BOLT #5
publishes no test vectors beyond Appendix A's weights, which are
checked; the tests also check HTLC transactions and preimage extraction
against BOLT #3's Appendix C and F vectors.

See [Status](#status) for what isn't implemented.

## Security

This library has received no security audit. It handles no secret
material: it builds unsigned transactions and reads public witnesses.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bolt5
```

to get a REPL for the main library.

[bolt5]: https://github.com/lightning/bolts/blob/master/05-onchain.md
[bolt3]: https://git.ppad.tech/bolt3
[spec]: https://github.com/lightning/bolts/tree/1aadb719b4007c4cea0ba6e36b08c4fb53788dee
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
