{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (foldM)
import qualified Data.List.Extra as LE
import Data.Text (Text)
import Data.Text.IO as TIO (putStrLn)
import Streaming as S (Of, Stream, chunksOf, mapsM)
import qualified Streaming.Prelude as S
import System.Environment (getArgs)
import TextShow (TextShow (showt))

-----------
-- Lists --
-----------

withTab ∷ Int → Text
withTab num = showt num <> "\t"

tabulateL ∷ Int → [Int] → [Text]
tabulateL cols ns = map mconcat $ LE.chunksOf cols $ map withTab ns

-- The integer list will be used twice and so the list will stay in memory.
sumAndTabL ∷ Int → [Int] → IO Int
sumAndTabL cols ns = do
  mapM_ TIO.putStrLn $ tabulateL cols ns
  pure $ sum ns

-- The integer list will be used only once.
-- We don't need to store the whole list in memory.
-- But summation and tabulation are now mixed into one function ⇒ no composability.
sumAndTabL1 ∷ Int → [Int] → IO Int
sumAndTabL1 cols ns = foldM prtLineSum 0 $ LE.chunksOf cols ns
  where
    prtLineSum !acc xs = do
      TIO.putStrLn $ mconcat $ map withTab xs
      pure $ acc + sum xs

-------------
-- Streams --
-------------

tabulateS ∷ Int → Stream (Of Int) IO r → Stream (Of Text) IO r
tabulateS cols str = mapsM S.mconcat $ S.chunksOf cols $ S.map withTab str

sumAndTabS ∷ Int → Stream (Of Int) IO r → IO Int
sumAndTabS cols = fmap S.fst' . S.mapM_ TIO.putStrLn . tabulateS cols . S.store S.sum

main ∷ IO ()
main = do
  args ← getArgs
  case args of
    ["-l"] → sumAndTabL 5 [1 .. 300000] >>= print ----------- using lists
    ["-l1"] → sumAndTabL1 5 [1 .. 300000] >>= print --------- using lists
    ["-s"] → sumAndTabS 5 (S.each [1 .. 300000]) >>= print -- using streams
    _ → TIO.putStrLn "Unsupported args"

-- 1	2	3
-- 4	5	6
-- 7	8	9
-- 10
-- 55
