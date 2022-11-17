import Data.Char (ord)
import Streaming (MonadIO, Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as S

printChar ∷ Char → IO ()
printChar c = putChar c >> putChar ' '

printCode ∷ Char → IO ()
printCode = print . ord

printChars ∷ MonadIO m ⇒ Stream (Of Char) m r → m r
printChars = S.mapM_ (S.liftIO . printChar)

printCodes ∷ MonadIO m ⇒ Stream (Of Char) m r → m r
printCodes = S.mapM_ (S.liftIO . printCode)

main ∷ IO ()
main = printChars $ printCodes $ S.copy $ S.each "hello"
