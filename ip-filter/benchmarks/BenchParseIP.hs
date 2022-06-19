module BenchParseIP where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import Data (iptexts)
import NFUtils ()
import ParseIP (parseIP, parseIPIter, parseIPIterStrict, parseIPMonadic)

benchParseIP :: [Benchmark]
benchParseIP =
  [ bench "parseIP/current" $ nf (map parseIP) iptexts,
    bgroup
      "parseIP"
      [ bench "monadic" $ nf (map parseIPMonadic) iptexts,
        bench "iterative" $ nf (map parseIPIter) iptexts,
        bench "iterative-strict" $ nf (map parseIPIterStrict) iptexts
      ]
  ]
