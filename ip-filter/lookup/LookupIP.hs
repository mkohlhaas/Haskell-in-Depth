module LookupIP where

import Data.List (find)
import IPTypes (IP, IPRange (IPRange), IPRangeDB (..))
import Data.Maybe (isJust)

isIpInRange ∷ IPRangeDB → IP → Bool
isIpInRange (IPRangeDB ips) ip = isJust $ find (inRange ip) ips
  where
    inRange ip (IPRange beg end) = beg <= ip && ip <= end

reportIPs ∷ IPRangeDB → [IP] → String
reportIPs iprdb = unlines . map go
  where
    go ip = show ip ++ ": " ++ show (isIpInRange iprdb ip)
