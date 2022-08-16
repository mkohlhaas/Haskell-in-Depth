import Test.DocTest

main ∷ IO ()
main = doctest ["-i/lookup", "lookup/ParseIP.hs"]
