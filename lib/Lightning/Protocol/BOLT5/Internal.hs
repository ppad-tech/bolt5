{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module: Lightning.Protocol.BOLT5.Internal
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Internal definitions for BOLT #5.
--
-- This module re-exports all constructors from the Types
-- module, including those not exported by the public API.
-- Use only in tests or trusted internal code.

module Lightning.Protocol.BOLT5.Internal (
    -- * All type constructors (for test use)
    CloseType(..)
  , UnresolvedOutput(..)
  , OutputResolution(..)
  , HTLCOutputType(..)
  , SpendingTx(..)
  , RevokedOutput(..)
  , RevokedOutputType(..)
  , PenaltyContext(..)
  ) where

import Lightning.Protocol.BOLT5.Types
