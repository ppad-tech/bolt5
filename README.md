# ppad-bolt5

[![](https://img.shields.io/hackage/v/ppad-bolt5?color=blue)](https://hackage.haskell.org/package/ppad-bolt5)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt5-lightblue)](https://docs.ppad.tech/bolt5)

A pure Haskell implementation of [BOLT #5][bolt5] (Lightning Network
on-chain transaction handling), including logic for mutual close,
unilateral close, and revoked transaction close scenarios.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > import qualified Data.ByteString as BS
  > import qualified Lightning.Protocol.BOLT3 as BOLT3
  > import qualified Lightning.Protocol.BOLT5 as BOLT5
  >
  > -- fee calculation using BOLT #5 Appendix A weight constants
  > let feerate = BOLT3.FeeratePerKw 5000
  > BOLT5.spending_fee feerate BOLT5.to_local_penalty_input_weight
  Satoshi 1620
  > BOLT5.spending_fee feerate BOLT5.penalty_tx_base_weight
  Satoshi 1070
  >
  > -- check if an HTLC has timed out at the current block height
  > let htlc = BOLT3.HTLC BOLT3.HTLCOffered (BOLT3.MilliSatoshi 50000000)
  >              (BOLT3.PaymentHash (BS.replicate 32 0xAA))
  >              (BOLT3.CltvExpiry 800000)
  > BOLT5.htlc_timed_out 799999 htlc
  False
  > BOLT5.htlc_timed_out 800000 htlc
  True
  >
  > -- extract a payment preimage from an offered HTLC witness
  > let preimage = BS.replicate 32 0xBB
  > let sig = BS.replicate 71 0xCC
  > let wit = BOLT3.Witness [sig, preimage]
  > BOLT5.extract_preimage_offered wit
  Just PaymentPreimage <redacted>
  >
  > -- construct an unsigned spending tx for a to_local output
  > let Just txid = BOLT3.mkTxId (BS.replicate 32 0x01)
  > let outpoint = BOLT3.OutPoint txid 0
  > let value = BOLT3.Satoshi 1000000
  > let revpk = BOLT3.RevocationPubkey (BOLT3.Pubkey (BS.replicate 33 0x02))
  > let delay = BOLT3.ToSelfDelay 144
  > let delayedpk = BOLT3.LocalDelayedPubkey (BOLT3.Pubkey (BS.replicate 33 0x03))
  > let dest = BOLT3.Script (BS.replicate 34 0x00)
  >
  > let stx = BOLT5.spend_to_local outpoint value revpk delay
  >             delayedpk dest feerate
  > BOLT5.stx_sighash_type stx
  SIGHASH_ALL
```

## Documentation

Haddocks are hosted at [docs.ppad.tech/bolt5][hadoc].

## Security

This is a pre-release and makes no claims about security whatsoever.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal build
$ cabal test
$ cabal bench
```

[bolt5]: https://github.com/lightning/bolts/blob/master/05-onchain.md
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/bolt5
