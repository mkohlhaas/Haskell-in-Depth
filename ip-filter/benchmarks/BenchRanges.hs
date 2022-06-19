module BenchRanges where

import Criterion.Main (Benchmark, bench, bgroup, env, nf, nfIO)
import Data (readIPRDBFile)
import NFUtils ()
import ParseIP (parseIPRange)

benchRanges :: [Benchmark]
benchRanges =
  [ bgroup
      "ranges"
      [ bgroup "read" $ map (\(desc, fname) -> bench desc $ nfIO (readIPRDBFile fname)) rangeFiles,
        bgroup "parse" $ map (\(desc, fname) -> env (readIPRDBFile fname) $ \iprdbf -> bench desc $ nf parseIPRange iprdbf) rangeFiles
      ]
  ]
  where
    rangeFiles =
      [ ("small", "1.iprs"),
        ("middle-sized", "2.iprs"),
        ("large", "3.iprs")
      ]
