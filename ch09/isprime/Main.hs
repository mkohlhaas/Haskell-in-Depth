import System.Environment
import System.TimeIt

import IsPrime

main ∷ IO ()
main = getArgs >>= timeIt . print . isPrime . read . head
