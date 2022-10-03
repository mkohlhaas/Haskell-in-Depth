module Data where

import Data.Maybe (fromJust)
import IPTypes (IP, IPRangeDB)
import ParseIP (parseIP, parseValidIPRanges)
import Paths_ip_filter (getDataFileName)

iptexts ∷ [String]
iptexts =
  [ "0.0.0.1",
    "192.168.1.1",
    "17.0.32.2",
    "255.255.252.41",
    "255.255.252.42"
  ]

ips ∷ [(String, IP)]
ips = map (\s → (s, fromJust $ parseIP s)) iptexts

readIPRDBFile ∷ FilePath → IO String
readIPRDBFile fname = getDataFileName (ipBenchDir <> fname) >>= readFile
  where
    ipBenchDir = "data/benchmarks/"

iprdb ∷ IO IPRangeDB
iprdb = parseValidIPRanges <$> readIPRDBFile "3.iprs"
