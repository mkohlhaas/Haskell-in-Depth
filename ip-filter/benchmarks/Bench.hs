--import BenchBuildIP
import BenchBuildIPGroups (benchBuildIP)
import BenchLookupIP (benchLookupIP)
import BenchParseIP (benchParseIP)
import BenchRanges (benchRanges)
import Criterion.Main (defaultMain)

main :: IO ()
main = defaultMain benchmarks
  where
    benchmarks =
      benchBuildIP -- <> bench_buildIP_list
        <> benchParseIP
        <> benchRanges
        <> benchLookupIP
