module LookupIPSpec where

import IPTypes (IP (IP), IPRange (IPRange), IPRangeDB (IPRangeDB))
import LookupIP (isIpInRange)
import Test.Tasty.Hspec (Spec, describe, it, shouldNotSatisfy, shouldSatisfy)

lookupIPSpecs ∷ Spec
lookupIPSpecs = describe "LookupIP" $ do
  spec_lookupIP

spec_lookupIP ∷ Spec
spec_lookupIP =
  describe "lookupIP" $ do
    let empty_iprdb = IPRangeDB []
        sample_iprdb = IPRangeDB [IPRange (IP 0) (IP 1), IPRange (IP 100) (IP 120)]
        ip1 = IP 110
        ip2 = IP 50
    it "no IP in empty list" $ ip1 `shouldNotSatisfy` isIpInRange empty_iprdb
    it "IP in sample list" $ ip1 `shouldSatisfy` isIpInRange sample_iprdb
    it "no IP in sample list" $ ip2 `shouldNotSatisfy` isIpInRange sample_iprdb
