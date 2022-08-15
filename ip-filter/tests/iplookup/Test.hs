import GoldenTests (goldenTests)
import LookupIPSpec (lookupIPSpecs)
import ParseIPSpec (parseIPSpecs)
import Props (props)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpecs)

main ∷ IO ()
main = do
  specs ← concat <$> mapM testSpecs [parseIPSpecs, lookupIPSpecs]
  goldens ← goldenTests
  defaultMain
    ( testGroup
        "All Tests"
        [ testGroup "Specs" specs,
          testGroup "Properties" props,
          testGroup "Golden Tests" goldens
        ]
    )
