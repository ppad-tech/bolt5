# ppad-bolt5

Haskell implementation of BOLT #5 (Lightning Network on-chain transaction
handling).

Specification: https://github.com/lightning/bolts/blob/master/05-onchain.md

## Project Structure

- `lib/` - library source (Lightning.Protocol.BOLT5)
- `test/` - tests (tasty + tasty-hunit)
- `bench/` - benchmarks (criterion for timing, weigh for allocations)
- `etc/` - reference materials (BOLT spec)
- `flake.nix` - nix flake for dependency and build management
- `ppad-bolt5.cabal` - cabal package definition
- `CLAUDE.md` / `AGENTS.md` - keep these in sync

## Build and Test

Enter devshell and use cabal:

```
nix develop
cabal build
cabal test
cabal bench
```

Do not use stack. All dependency and build management via nix.

## Dependencies

### ppad libraries (use freely)

Use ppad libraries (github.com/ppad-tech, git.ppad.tech) liberally.

### External libraries

Use only minimal external dependencies. Prefer GHC's core/boot libraries
(base, bytestring, primitive, etc.).

**Ask for explicit confirmation before adding any library outside of:**
- GHC boot/core libraries
- ppad-* libraries
- Test dependencies (tasty, QuickCheck, etc. for test-suite only)
- Benchmark dependencies (criterion, weigh for benchmark only)

## Code Style

### Performance

- Use strictness annotations (BangPatterns) liberally
- Prefer UNPACK for strict record fields
- Use MagicHash, UnboxedTuples, GHC.Exts for hot paths
- Do not rely on UNBOX pragmas; implement primitives directly with
  MagicHash and GHC.Exts when needed
- Use INLINE pragmas for small functions
- Refer to ppad-sha256 and ppad-fixed for low-level patterns

### Type safety

- Encode invariants into the type system
- Use newtypes liberally
- Use ADTs to make illegal states unrepresentable
- Prefer smart constructors that validate inputs

### Safety

- Never use partial Prelude functions (head, tail, !!, etc.)
- Avoid brittle partials in tests too (e.g., unchecked indexing). Prefer
  bounds checks or total helpers even in test code.
- Avoid non-exhaustive pattern matches and unsafe behavior; use total
  helpers and make all constructors explicit.
- Use Maybe/Either for fallible operations
- Validate all inputs at system boundaries

### Formatting

- Keep lines under 80 characters
- Use Haskell2010
- Module header with copyright, license, maintainer
- OPTIONS_HADDOCK prune for public modules
- Haddock examples for exported functions

## Testing

Use tasty to wrap all tests:
- tasty-hunit for unit tests with known vectors
- tasty-quickcheck for property-based tests
- Source test vectors from specifications (RFC, BOLT spec, Wycheproof, etc.)

Property tests should enforce invariants that can't be encoded in types.

## Benchmarking

Always maintain benchmark suites:
- `bench/Main.hs` - criterion for wall-time benchmarks
- `bench/Weight.hs` - weigh for allocation tracking

Define NFData instances for types that need benchmarking.

## Git Workflow

- Feature branches for development; commit freely there
- Logical, atomic commits on feature branches
- Master should be mostly merge commits
- Merge to master with `--no-ff` after validation
- Always build and test before creating a merge commit
- Write detailed merge commit messages summarising changes

### Worktree flow (for planned work)

When starting work on an implementation plan:

```
git worktree add ./impl-<desc> -b impl/<desc> master
# work in that worktree
# merge to master when complete
git worktree remove ./impl-<desc>
```

### Commits

- Higher-level descriptions in merge commits
- Never update git config
- Never use destructive git commands (push --force, hard reset) without
  explicit request
- Never skip hooks unless explicitly requested

## Planning

When planning work:
- Highlight which steps can be done independently
- Consider forking subagents for concurrent work on independent steps
- Write implementation plans to `plans/IMPL<n>.md` if the project uses
  this convention

## Flake Structure

The flake.nix follows ppad conventions:
- Uses ppad-nixpkgs as base
- Follows references to avoid duplication
- Supports LLVM backend via cabal flag
- Provides devShell with ghc, cabal, cc, llvm
