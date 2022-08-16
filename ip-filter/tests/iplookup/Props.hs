module Props where

import qualified FastLookup as FL
import GenIP (genIP, genIPComponents, genIPRange, genIPRangeDB, genInvalidIPRange)
import Hedgehog (Property, assert, forAll, property, tripping, (===))
import qualified Hedgehog.Gen as Gen
import IPTypes (IPRange (IPRange), IPRangeDB (IPRangeDB))
import LookupIP (isIpInRange)
import ParseIP (buildIP, buildIPFoldl, buildIPFoldlShl, buildIPFoldr, parseIP, parseIPRange, parseIPRanges)
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

prop_buildIPs ∷ Property
prop_buildIPs = property $ do
  ipcs ← forAll genIPComponents
  let ip = buildIP ipcs
  buildIPFoldr ipcs === ip
  buildIPFoldl ipcs === ip
  buildIPFoldlShl ipcs === ip

prop_parseIP ∷ Property
prop_parseIP = property $ do
  ip ← forAll genIP
  parseIP (show ip) === Just ip

prop_parseIP_show ∷ Property
prop_parseIP_show = property $ do
  ip ← forAll genIP
  tripping ip show parseIP

prop_parseIPRange_show ∷ Property
prop_parseIPRange_show = property $ do
  ipr ← forAll genIPRange
  tripping ipr show parseIPRange

prop_parseIPRanges_show ∷ Property
prop_parseIPRanges_show = property $ do
  iprdb ← forAll genIPRangeDB
  tripping iprdb show parseIPRanges

prop_no_parseInvalidIPRange ∷ Property
prop_no_parseInvalidIPRange = property $ do
  inv_ip ← forAll genInvalidIPRange
  parseIPRange (show inv_ip) === Nothing

prop_isIpInRange_empty ∷ Property
prop_isIpInRange_empty = property $ do
  ip ← forAll genIP
  assert (not $ isIpInRange (IPRangeDB []) ip)

prop_isIpInRange_bordersIncluded ∷ Property
prop_isIpInRange_bordersIncluded = property $ do
  iprdb@(IPRangeDB iprdbs) ← forAll genIPRangeDB
  IPRange ip1 ip2 ← forAll $ Gen.element iprdbs
  assert (isIpInRange iprdb ip1)
  assert (isIpInRange iprdb ip2)

prop_isIpInRanges_agree ∷ Property
prop_isIpInRanges_agree = property $ do
  iprdb ← forAll genIPRangeDB
  let fiprdb = FL.fromIPRangeDB iprdb
  ip ← forAll genIP
  assert (isIpInRange iprdb ip == FL.isIpInRange fiprdb ip)

props ∷ [TestTree]
props =
  [ testProperty "buildIP implementations agrees with each other" prop_buildIPs,
    testProperty "parseIP works as expected" prop_parseIP,
    testProperty "parseIP agrees with show" prop_parseIP_show,
    testProperty "parseIPRange agrees with show" prop_parseIPRange_show,
    testProperty "parseIPRanges agrees with show" prop_parseIPRanges_show,
    testProperty "no parse of invalid IP ranges" prop_no_parseInvalidIPRange,
    testProperty "no ip in empty list" prop_isIpInRange_empty,
    testProperty "isIpInRange includes borders" prop_isIpInRange_bordersIncluded,
    testProperty "isIpInRange agrees with fast isIpInRange" prop_isIpInRanges_agree
  ]
