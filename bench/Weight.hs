{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Lightning.Protocol.BOLT5 as BOLT5
import Weigh

-- note that 'weigh' doesn't work properly in a repl
main :: IO ()
main = mainWith placeholder

placeholder :: Weigh ()
placeholder = wgroup "placeholder" $
  func "placeholder" (const BOLT5.placeholder) ()
