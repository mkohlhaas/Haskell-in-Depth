module BenchBuildIP where

import Criterion.Main (Benchmark, bench, nf, whnf)
import NFUtils ()
import ParseIP (buildIPFoldl, buildIPFoldlShl, buildIPFoldr)

benchBuildIP :: [Benchmark]
benchBuildIP =
  [ bench "buildIP-foldr" $ whnf buildIPFoldr theip,
    bench "buildIP-foldl" $ whnf buildIPFoldl theip,
    bench "buildIP-foldl-shl" $ whnf buildIPFoldlShl theip
  ]
  where
    theip = [17, 0, 32, 2]

benchBuildIPList :: [Benchmark]
benchBuildIPList =
  [ bench "buildIP-foldr" $ nf (map buildIPFoldr) ipcomps,
    bench "buildIP-foldl" $ nf (map buildIPFoldl) ipcomps,
    bench "buildIP-foldl-shl" $ nf (map buildIPFoldlShl) ipcomps
  ]
  where
    ipcomps =
      [ [0, 0, 0, 1],
        [192, 168, 1, 1],
        [255, 255, 252, 41],
        [255, 255, 252, 41],
        [17, 0, 32, 2]
      ]
