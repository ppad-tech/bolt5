# ARCH plan v1

## Goals

- Implement BOLT #5 on-chain handling for mutual, unilateral, revoked
  closes.
- Encode invariants in types; total, safe API.
- Provide pure resolution engine that consumes chain events and returns
  actions to take (spend, watch, wait).
- Keep dependencies minimal; prefer ppad libraries and GHC boot libs.
- Implement tx primitives locally (Bitcoin.Prim.Tx) for later extraction
  into ppad-tx.

## Non-goals (v1)

- Wallet integration, mempool publishing, or networking.
- Full transaction construction for every policy variant unless
  needed by spec examples.
- Persistent storage or chain sync.

## Public API shape (lib/Lightning/Protocol/BOLT5)

- Re-export primary types and functions from internal modules.
- Keep a small surface:
  - Channel parameters and state
  - On-chain events (funding spent, commitment seen, spend seen)
  - Resolution actions (spend tx, watch, wait, fail channel)
  - HTLC tx generation helpers

## Module layout

### Tx primitives (Bitcoin.Prim.*)

These modules follow ppad-script conventions and will later be extracted
to ppad-tx:

- Bitcoin.Prim.Tx
  - Tx, TxIn, TxOut, OutPoint, Witness types
  - Serialisation (to/from ByteArray, base16)
  - TxId computation (double SHA256 of serialised tx)
- Bitcoin.Prim.Tx.Sighash
  - SIGHASH flags, sighash computation for legacy/segwit

### BOLT #5 modules (Lightning.Protocol.BOLT5.*)

- Lightning.Protocol.BOLT5
  - public re-exports, minimal glue
- Lightning.Protocol.BOLT5.Types
  - newtypes, ADTs, smart constructors
- Lightning.Protocol.BOLT5.Channel
  - channel parameters/state, validation
- Lightning.Protocol.BOLT5.OnChain
  - resolution engine per BOLT #5
- Lightning.Protocol.BOLT5.HTLC
  - HTLC tx generation, offered/received logic
- Lightning.Protocol.BOLT5.Weight
  - weight calculations, constants

(Keep clear separation between tx primitives and Lightning-specific
logic; tx modules should have no Lightning dependencies.)

## Core types and invariants

- newtype Satoshi, BlockHeight, BlockDepth, CSVDelay, Weight.
- newtype TxId, OutPoint, TxIndex for on-chain references.
- data Role = Local | Remote.
- data CommitmentKind = LocalCommit | RemoteCommit | RevokedCommit.
- data OutputKind = ToLocal | ToRemote | AnchorLocal | AnchorRemote
  | HTLCOffered | HTLCReceived.
- data HTLC with amount, payment hash, cltv expiry, offerer.
- data CommitmentView summarizes a commitment tx:
  - outputs list with kind, amount, scripts, htlc id
- data OnChainEvent:
  - FundingSpent ClosingTx
  - CommitmentSeen CommitmentKind CommitmentView
  - SpendSeen OutPoint SpendingTx
- data ResolutionAction:
  - Publish Tx
  - Watch OutPoint
  - WaitUntil BlockHeight
  - FailChannel Reason

Smart constructors enforce:
- Amounts >= 0 and within dust rules.
- HTLC CLTV within bounds.
- CSV delays non-negative.

## Resolution engine responsibilities

- Given channel params + current state + event, derive required
  actions per BOLT #5.
- Track unresolved outputs and allow re-entry on reorgs.
- Local/remote symmetry: logic should be parametric on Role.
- Separate "detection" (what tx is this) from "resolution" (what to do).

## Dependency strategy

- Use ppad-script for Script representation.
- Use ppad-sha256 for txid computation (double SHA256).
- Implement tx primitives (Bitcoin.Prim.Tx) locally; extract to ppad-tx
  later. This will unify duplicated types across bolt2/3/7 (TxId,
  Outpoint, Satoshi, etc.).
- Avoid new deps; ask before adding any non-boot libraries.
- Use ByteString for hashes; keep MagicHash for hot paths only.

## Testing strategy

- Unit tests with vectors from BOLT #5 (HTLC outputs, delays).
- Property tests for invariants:
  - All unresolved outputs produce actions
  - Symmetry between local/remote roles
  - Idempotent handling across reorg replay

## Benchmarks

- Criterion benchmarks for resolution engine on varied HTLC counts.
- Weigh benchmarks for allocation hot spots in HTLC generation.

## Concurrency notes (for agent delegation)

- Tx primitives (Bitcoin.Prim.Tx) are fully independent; start first.
- Types + Channel params can be built once tx primitives exist.
- Weight calculations and HTLC tx generation are independent.
- On-chain resolution can be built once types are fixed.
- Tests and benches can proceed after public API stabilizes.
