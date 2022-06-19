module GoldenTests (goldenTests) where

import LookupIP (reportIPs)
import ParseIP (parseValidIPRanges, parseValidIPs)
import System.FilePath (normalise, replaceExtension, takeBaseName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile, writeBinaryFile)

goldenTests :: IO [TestTree]
goldenTests = sequence [goldenLookupIP]

testsDir :: FilePath
testsDir = normalise "data/tests/iplookup/"

goldenLookupIP :: IO TestTree
goldenLookupIP = testGroup "lookupIP" . map createTest <$> findByExtension [".iprs"] testsDir

createTest :: FilePath -> TestTree
createTest iprsf = goldenVsFile (takeBaseName iprsf) goldenf outf testAction
  where
    ipsf = replaceExtension iprsf ".ips"
    goldenf = replaceExtension iprsf ".out.golden"
    outf = replaceExtension iprsf ".out"
    testAction = do
      iprs <- parseValidIPRanges <$> readFile iprsf
      ips <- parseValidIPs <$> readFile ipsf
      writeBinaryFile outf $ reportIPs iprs ips
