import Graphs (Edge (MkEdge1, MkEdge2), g1, g2, isEdgeLoopy, neighbors)
import SimplifyWiden (Simplifier (simplify), Widener (widen))
import Unescape (uprint)
import XListable (testXList)

testGraphs ∷ IO ()
testGraphs = do
  print $ neighbors g1 0
  print $ isEdgeLoopy g1 (MkEdge1 0 1)
  print $ neighbors g2 "A"
  print $ isEdgeLoopy g2 (MkEdge2 "A" "B")

main ∷ IO ()
main = do
  testGraphs
  print $ simplify True ++ " " ++ widen 'x' ------ "True x"
  print $ simplify answer + widen (widen False) -- 42
  uprint "Виталий" ------------------------------- "Vitali"
  uprint "Привет, мир!" -------------------------- "Привет, мир!"
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

-- []
-- False
-- []
-- False
-- Output:
-- "True x"
-- 42
-- "Виталий"
-- "Привет, мир!"
-- True
