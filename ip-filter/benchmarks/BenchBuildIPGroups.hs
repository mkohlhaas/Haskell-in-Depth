module BenchBuildIPGroups where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import NFUtils ()
import ParseIP (buildIP, buildIPFoldl, buildIPFoldlShl, buildIPFoldr)

benchBuildIP ∷ [Benchmark]
benchBuildIP =
  [ bgroup
      "buildIP"
      [ let theip = [17, 0, 32, 2]
         in bgroup -- building a hierarchy of benchmarks with groups
              "single"
              [ bench "default" $ nf buildIP theip,
                bench "foldr" $ nf buildIPFoldr theip,
                bench "foldl" $ nf buildIPFoldl theip,
                bench "foldl-shl" $ nf buildIPFoldlShl theip
              ],
        let ipcomps =
              [ [0, 0, 0, 1],
                [192, 168, 1, 1],
                [17, 0, 32, 2],
                [255, 255, 252, 41],
                [255, 255, 252, 41]
              ]
         in bgroup
              "several"
              [ bench "default" $ nf (map buildIP) ipcomps,
                bench "foldr" $ nf (map buildIPFoldr) ipcomps,
                bench "foldl" $ nf (map buildIPFoldl) ipcomps,
                bench "foldl-shl" $ nf (map buildIPFoldlShl) ipcomps
              ]
      ]
  ]
