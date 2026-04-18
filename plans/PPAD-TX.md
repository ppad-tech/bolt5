# ppad-tx

Minimal Bitcoin transaction primitives for ppad libraries.

## Motivation

Multiple ppad-bolt implementations duplicate core tx-related types:

| Type             | bolt2 | bolt3 | bolt7 |
|------------------|-------|-------|-------|
| TxId             | yes   | yes   | -     |
| Outpoint         | yes   | yes   | -     |
| Sequence         | -     | yes   | -     |
| Locktime         | -     | yes   | -     |
| Script           | -     | yes   | -     |
| Witness          | -     | yes   | -     |
| Satoshi(s)       | yes   | yes   | -     |
| MilliSatoshi(s)  | yes   | yes   | -     |
| Point/Pubkey     | yes   | yes   | yes   |
| Signature        | yes   | -     | yes   |
| ChainHash        | yes   | -     | yes   |
| ShortChannelId   | yes   | -     | yes   |

Common gap across all: no raw Tx structure, no serialisation, no txid
computation from raw bytes.

ppad-tx will provide canonical definitions and allow bolt impls to depend
on a single source.

## Scope

### In scope (v1)

- Raw transaction types (Tx, TxIn, TxOut)
- Outpoint, Sequence, Locktime
- Serialisation to/from bytes (legacy and segwit formats)
- TxId computation (double SHA256 of non-witness serialisation)
- Sighash computation (legacy and BIP143 segwit)
- Basic amount types (Satoshi)

### Out of scope (v1)

- Script execution or validation
- Signature creation/verification (use ppad-secp256k1)
- Transaction building DSL
- PSBT support
- Taproot/BIP341 sighash (defer to v2)

## Module layout

```
lib/
  Bitcoin/
    Prim/
      Tx.hs           -- core types, serialisation, txid
      Tx/
        Sighash.hs    -- sighash computation
```

Follow ppad-script conventions:
- `Bitcoin.Prim.*` namespace
- ByteArray for internal representation where appropriate
- base16 conversion utilities
- OPTIONS_HADDOCK prune

## Core types

```haskell
-- | Transaction ID (32 bytes, little-endian double-SHA256).
newtype TxId = TxId BS.ByteString

-- | Transaction outpoint.
data OutPoint = OutPoint
  { op_txid  :: {-# UNPACK #-} !TxId
  , op_vout  :: {-# UNPACK #-} !Word32
  }

-- | Transaction input.
data TxIn = TxIn
  { txin_prevout    :: {-# UNPACK #-} !OutPoint
  , txin_script_sig :: !BS.ByteString
  , txin_sequence   :: {-# UNPACK #-} !Word32
  }

-- | Transaction output.
data TxOut = TxOut
  { txout_value         :: {-# UNPACK #-} !Word64  -- satoshis
  , txout_script_pubkey :: !BS.ByteString
  }

-- | Witness stack for a single input.
newtype Witness = Witness [BS.ByteString]

-- | Complete transaction.
data Tx = Tx
  { tx_version  :: {-# UNPACK #-} !Word32
  , tx_inputs   :: ![TxIn]
  , tx_outputs  :: ![TxOut]
  , tx_witnesses :: ![Witness]  -- empty list for legacy tx
  , tx_locktime :: {-# UNPACK #-} !Word32
  }
```

## Serialisation

### Format detection

- Legacy: version || inputs || outputs || locktime
- Segwit: version || 0x00 || 0x01 || inputs || outputs || witnesses || locktime

Detect segwit by marker byte (0x00) after version. If present and followed
by flag (0x01), parse as segwit.

### Public API

```haskell
-- Serialisation
to_bytes   :: Tx -> BS.ByteString      -- segwit format if witnesses present
from_bytes :: BS.ByteString -> Maybe Tx

to_base16   :: Tx -> BS.ByteString
from_base16 :: BS.ByteString -> Maybe Tx

-- Legacy serialisation (for txid computation)
to_bytes_legacy :: Tx -> BS.ByteString

-- TxId
txid :: Tx -> TxId  -- double SHA256 of legacy serialisation
```

### Encoding details

All integers little-endian. Variable-length integers (compactSize):
- 0x00-0xfc: 1 byte
- 0xfd: 0xfd || 2 bytes (little-endian)
- 0xfe: 0xfe || 4 bytes
- 0xff: 0xff || 8 bytes

## Sighash

### Flags

```haskell
data SighashType
  = SIGHASH_ALL
  | SIGHASH_NONE
  | SIGHASH_SINGLE
  | SIGHASH_ALL_ANYONECANPAY
  | SIGHASH_NONE_ANYONECANPAY
  | SIGHASH_SINGLE_ANYONECANPAY
```

### Legacy sighash

Per BIP-143 predecessor. Modify tx copy based on flags, append sighash
type as 4-byte LE, double SHA256.

### BIP143 segwit sighash

Required for signing segwit inputs. Precomputed:
- hashPrevouts: SHA256(SHA256(all input outpoints))
- hashSequence: SHA256(SHA256(all input sequences))
- hashOutputs: SHA256(SHA256(all outputs))

Then:
```
version || hashPrevouts || hashSequence || outpoint || scriptCode ||
value || sequence || hashOutputs || locktime || sighashType
```

### Public API

```haskell
-- Legacy
sighash_legacy
  :: Tx
  -> Int              -- input index
  -> BS.ByteString    -- scriptPubKey being spent
  -> SighashType
  -> BS.ByteString    -- 32-byte hash

-- BIP143 segwit
sighash_segwit
  :: Tx
  -> Int              -- input index
  -> BS.ByteString    -- scriptCode
  -> Word64           -- value being spent
  -> SighashType
  -> BS.ByteString    -- 32-byte hash
```

## Dependencies

Minimal:
- base
- bytestring
- primitive (for ByteArray if needed)
- ppad-sha256 (for txid and sighash)
- ppad-base16 (for hex conversion)

## Testing

- Known tx vectors from Bitcoin Core / BIPs
- Round-trip: from_bytes . to_bytes == id
- TxId computation against known txids
- Sighash vectors from BIP143

Sources:
- BIP143 test vectors
- Bitcoin Core's tx_valid.json / tx_invalid.json
- Manually constructed edge cases (empty witness, max inputs, etc.)

## Implementation steps

### Step 1: Core types + serialisation (independent)

- Define Tx, TxIn, TxOut, OutPoint, Witness
- Implement compactSize encoding/decoding
- Implement to_bytes / from_bytes (both formats)
- Implement txid computation

### Step 2: Sighash (depends on Step 1)

- Define SighashType
- Implement legacy sighash
- Implement BIP143 segwit sighash

### Step 3: Tests + benchmarks

- Add tx serialisation round-trip tests
- Add known vector tests for txid
- Add BIP143 sighash vectors
- Criterion benchmarks for serialisation
- Weigh benchmarks for allocations

### Step 4: Polish

- Haddock documentation with examples
- Ensure line length < 80
- Module headers, OPTIONS_HADDOCK prune

## Future (v2)

- BIP341 taproot sighash
- BIP340 schnorr signature integration
- Witness program version detection
- Transaction weight/vsize calculation
