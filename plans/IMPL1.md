# IMPL plan v1

## Step 0: Recon (independent)

- Inventory ppad-* libraries for tx/script/crypto helpers.
- Review existing BOLT implementations for duplicated types:
  - bolt2/Types.hs: TxId, Outpoint, Satoshis, MilliSatoshis, Point,
    Signature, ScriptPubKey, ChainHash, ShortChannelId.
  - bolt3/Types.hs: TxId, Outpoint, Sequence, Locktime, Script, Witness,
    Satoshi, MilliSatoshi, Point, Pubkey, PaymentHash.
  - bolt3/Tx.hs: Lightning-specific tx builders (CommitmentTx, HTLCTx,
    ClosingTx) but NOT raw Bitcoin tx serialisation.
  - bolt7/Types.hs: ChainHash, ShortChannelId, Signature, Point.
  - Common gap: no raw Tx structure, no serialisation, no txid computation.
- Review ppad-script (../script) for module conventions.
- Confirm which variants to support: anchors, no-anchors, etc.

NOTE: Significant type duplication across bolt impls. Future ppad-tx
extraction should unify these; for now, implement fresh in Bitcoin.Prim.Tx
with eye toward becoming the canonical source.

## Step 1: Tx primitives (independent)

- Create `lib/Bitcoin/Prim/Tx.hs` following ppad-script conventions.
- Implement core types (filling gaps from bolt3/Types.hs):
  - Tx (version, inputs, outputs, locktime, witnesses)
  - TxIn (outpoint, scriptSig, sequence)
  - TxOut (value, scriptPubKey)
- Reuse where sensible:
  - TxId, Outpoint, Sequence, Locktime patterns from bolt3/Types.hs
  - Or define fresh and unify later during ppad-tx extraction.
- Serialisation (the key missing piece):
  - to_bytes / from_bytes (raw tx format)
  - to_base16 / from_base16
  - Segwit marker/flag handling (0x00 0x01 prefix)
- TxId computation (double SHA256 of non-witness serialisation).
- Keep this module independent of Lightning; will later become ppad-tx.

## Step 1b: Sighash (depends on Step 1)

- Create `lib/Bitcoin/Prim/Tx/Sighash.hs`.
- SIGHASH flags (ALL, NONE, SINGLE, ANYONECANPAY).
- Legacy sighash computation.
- BIP143 segwit sighash computation (needed for commitment tx signing).

## Step 2: BOLT5 types and invariants (depends on Step 1)

- Create `lib/Lightning/Protocol/BOLT5/Types.hs`.
- Implement newtypes and smart constructors:
  - Satoshi, Weight, BlockHeight, CSVDelay.
- Re-export or newtype-wrap TxId, OutPoint from Bitcoin.Prim.Tx.
- Define Role, CommitmentKind, OutputKind, HTLC.
- Provide total helpers; no partials.

## Step 3: Channel parameters + state (depends on Step 2)

- Create `lib/Lightning/Protocol/BOLT5/Channel.hs`.
- Define ChannelParams (dust limits, delays, feature flags).
- Define ChannelState (known commitments, unresolved outputs).
- Encode invariants (e.g., anchor vs non-anchor rules).

## Step 4: HTLC tx generation (depends on Steps 1-2)

- Create `lib/Lightning/Protocol/BOLT5/HTLC.hs`.
- Implement offered/received HTLC tx templates.
- Use Bitcoin.Prim.Tx for tx construction.
- Add weight helpers and witness size constants.

## Step 5: Weight calculations (depends on Step 2)

- Create `lib/Lightning/Protocol/BOLT5/Weight.hs`.
- Implement expected weights from BOLT #5 Appendix A.
- Provide functions to compute fee based on feerate.

## Step 6: On-chain resolution engine (depends on Steps 2-3)

- Create `lib/Lightning/Protocol/BOLT5/OnChain.hs`.
- Implement resolution rules for:
  - Mutual close
  - Local commitment
  - Remote commitment
  - Revoked commitment
- Track unresolved outputs and required actions.
- Handle reorg safety by making actions idempotent.

## Step 7: Public module + cabal glue (depends on Steps 1-6)

- Update `lib/Lightning/Protocol/BOLT5.hs` to re-export API.
- Update `ppad-bolt5.cabal` for new modules (including Bitcoin.Prim.*).
- Add ppad-script, ppad-sha256 dependencies.

## Step 8: Tests (depends on Steps 1-7)

- Replace placeholder tests in `test/Main.hs`.
- Add unit tests for tx serialisation (known vectors).
- Add tasty-hunit vectors from BOLT #5 text.
- Add tasty-quickcheck properties for invariants.

## Step 9: Benchmarks (depends on Steps 1-7)

- Update `bench/Main.hs` and `bench/Weight.hs`.
- Provide NFData instances for benchmarked types.
- Benchmark tx serialisation/deserialisation.

## Step 10: Docs and examples (parallel with Steps 7-9)

- Add Haddock examples for exported functions.
- Ensure module headers, line length, OPTIONS_HADDOCK prune.

## Delegation map

- Agent A: Steps 1-1b (Tx primitives + Sighash).
- Agent B: Steps 2-3 (BOLT5 Types + Channel).
- Agent C: Steps 4-5 (HTLC + Weight).
- Agent D: Step 6 (OnChain engine).
- Agent E: Steps 8-9 (Tests + Bench).
- Integrator: Step 7 + Step 10, resolve API mismatches.

## Future: ppad-tx extraction

Once BOLT5 is stable, extract Bitcoin.Prim.Tx* into standalone ppad-tx:
- Move lib/Bitcoin/Prim/Tx.hs and Sighash.hs to new repo.
- Update ppad-bolt5 to depend on ppad-tx.
- No API changes needed if module names are preserved.
