import Graphs (Edge (MkEdge1), g1, isEdgeLoopy, neighbors)
import SimplifyWiden (Simplifier (simplify), Widener (widen))
import Unescape (uprint)
import XListable (testXList)

testGraphs ∷ IO ()
testGraphs = do
  print $ neighbors g1 0
  print $ isEdgeLoopy g1 (MkEdge1 0 1)

main ∷ IO ()
main = do
  print $ simplify True ++ " " ++ widen 'x' ------ "True x"
  print $ simplify answer + widen (widen False) -- 42
  uprint "Виталий" -- "Vitali"
  uprint "Привет, мир!"
  print $ ---------------------------------------- True
    and
      [ testXList (),
        testXList True,
        testXList False,
        testXList 'x'
      ]
  where
    answer ∷ Integer
    answer = 42
