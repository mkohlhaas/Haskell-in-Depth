module BenchLookupIP where

import Criterion.Main (Benchmark, bench, bgroup, env, nf, whnf)
import Data (iprdb, ips)
import qualified FastLookup as FL
import LookupIP (isIpInRange)
import NFUtils ()

benchLookupIP ∷ [Benchmark]
benchLookupIP =
  [ env iprdb $ \iprdb' →
      bgroup
        "lookupIP"
        [ bgroup "single" $ map (\(textip, ip) → bench textip $ whnf (isIpInRange iprdb') ip) ips,
          bench "several" $ nf (map (isIpInRange iprdb')) $ map snd ips
        ],
    env iprdb $ \iprdb' →
      let fiprdb = FL.fromIPRangeDB iprdb'
       in bgroup
            "lookupIP (fast)"
            [ bgroup "single" $ map (\(textip, ip) → bench textip $ whnf (FL.isIpInRange fiprdb) ip) ips,
              bench "several" $ nf (map (FL.isIpInRange fiprdb)) $ map snd ips
            ]
  ]
