# ppad-bolt5

![](https://img.shields.io/badge/license-MIT-brightgreen)

A pure Haskell implementation of [BOLT #5][bolt5] (Lightning Network
on-chain transaction handling), including logic for mutual close,
unilateral close, and revoked transaction close scenarios.

## Documentation

Haddocks are hosted at [docs.ppad.tech/bolt5][hadoc].

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC.
If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

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
