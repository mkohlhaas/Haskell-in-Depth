{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Exception (throw, throwIO)
import Control.Monad.Catch (Exception, MonadCatch (catch), MonadThrow (throwM), try, tryJust)
import Data.Functor ((<&>))
import Data.Either (fromRight)

data MyArithException = DivByZero | OtherArithException
  deriving (Show, Exception)

mult ∷ Int → Int → Int
mult 0 _ = 0
mult a b = a * b

divPure ∷ Int → Int → Int
divPure _ 0 = throw DivByZero
divPure a b = a `div` b

divIO ∷ Int → Int → IO Int
divIO _ 0 = throwIO DivByZero
divIO a b = pure (a `div` b)

divM ∷ MonadThrow m ⇒ Int → Int → m Int
divM _ 0 = throwM DivByZero
divM a b = pure (a `div` b)

testComputation ∷ MonadThrow m ⇒ Int → Int → Int → m Int
testComputation a b c = divM a b >>= divM c

divTestWithRecovery ∷ Int → Int → Int → IO Int
divTestWithRecovery a b c = try (testComputation a b c) <&> (fromRight 0 ∷ Either MyArithException _ → _)

divTestWithRecovery2 ∷ Int → Int → Int → IO Int
divTestWithRecovery2 a b c = tryJust isDivByZero (testComputation a b c) <&> fromRight 0
  where
    isDivByZero DivByZero = Just ()
    isDivByZero _ = Nothing

divTestIO ∷ Int → Int → Int → IO Int
divTestIO a b c = testComputation a b c `catch` handler
  where
    handler ∷ MyArithException → IO Int
    handler e = putStrLn ("We've got an exception: " ++ show e ++ ". Using default value 0") >> pure 0

main ∷ IO ()
main = do
  divTestWithRecovery 10 0 2 >>= print ----- 0
  divTestWithRecovery2 10 0 2 >>= print ---- 0
  divTestIO 10 0 2 >>= print --------------- We've got an exception: DivByZero. Using default value 0
