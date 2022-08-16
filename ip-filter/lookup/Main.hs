module Main where

import Control.Monad.Catch (Handler (Handler), MonadThrow (throwM), catches)
import IPTypes (InvalidArgsException (InvalidIP, LoadIPRangesError))
import LookupIP (reportIPs)
import Options.Applicative (Parser)
import Options.Applicative as Opt (Parser, argument, execParser, fullDesc, help, helper, info, metavar, progDesc, str, (<**>))
import ParseIP (parseIP, parseIPRanges)
import System.Exit (ExitCode)

data Params
  = Params
      FilePath
      String

mkParams ∷ Parser Params
mkParams =
  Params
    <$> argument str (metavar "FILE" <> help "IP range database")
    <*> argument str (metavar "IP" <> help "IP address to check")

run ∷ Params → IO ()
run (Params fp ipstr) = do
  iprs ← parseIPRanges <$> readFile fp
  case (iprs, parseIP ipstr) of
    (_, Nothing) → throwM $ InvalidIP ipstr
    (Left pe, _) → throwM $ LoadIPRangesError pe
    (Right iprdb, Just ip) → putStrLn $ reportIPs iprdb [ip]

main ∷ IO ()
main = (execParser opts >>= run) `catches` [Handler parserExit]
  where
    opts =
      info
        (mkParams <**> helper)
        (fullDesc <> progDesc ("Answers True/False depending on whether " ++ "an IP address belongs to the IP range database"))
    parserExit ∷ ExitCode → IO ()
    parserExit _ = pure ()
