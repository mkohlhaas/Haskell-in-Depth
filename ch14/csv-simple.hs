{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Trans.Resource (runResourceT)
import Data.Attoparsec.ByteString.Char8 as A (Parser, char, endOfInput, endOfLine, manyTill, parseOnly, sepBy1, takeWhile)
import Data.Attoparsec.ByteString.Streaming (parsed)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Streaming.ByteString as BS
import qualified Streaming.Prelude as S

field ∷ Parser ByteString
field = A.takeWhile (\c → c /= ',' && c /= '\r' && c /= '\n')

textField ∷ Parser Text
textField = T.decodeUtf8 <$> field

record ∷ Parser [Text]
record = textField `sepBy1` char ','

-- >>> :type parseOnly
-- parseOnly ∷ Parser a → ByteString → Either String a

-- >>> parseOnly record "1,2,3"
-- Right ["1","2","3"]

endOfFile ∷ Parser ()
endOfFile = endOfInput <|> endOfLine *> endOfInput

-- The `manyTill` combinator applies the first combinator repeatedly until the second one is successfully applied.
-- (This program doesn't actually stream due to the implementation of this file parser combinator.
-- It waits for the end of file explicitly, keeping all the data in memory, and then produces a final result.)
file ∷ Parser [[Text]]
file = (:) <$> record <*> manyTill (endOfLine *> record) endOfFile

-- >>> :type parsed
--                                                                    `void` throws this result away
--                                                                ______________|_____________________
--                                                                |                                  |
-- parsed ∷ Monad m ⇒ Parser a → ByteString m r → Stream (Of a) m (Either (Message, ByteString m r) r)

main ∷ IO ()
main =
  runResourceT $
    BS.readFile "data/quotes.csv"
      & parsed file
      & void
      & S.print
