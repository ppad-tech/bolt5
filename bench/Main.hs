{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import qualified Lightning.Protocol.BOLT5 as BOLT5

main :: IO ()
main = defaultMain [
    placeholder
  ]

placeholder :: Benchmark
placeholder = bgroup "placeholder" [
    bench "placeholder" $ nf (const BOLT5.placeholder) ()
  ]
