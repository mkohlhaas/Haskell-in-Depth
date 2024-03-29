module Main where

import App (MyApp, runMyApp)
import Control.Exception (IOException)
import Control.Monad.Catch (Handler (Handler), MonadThrow (throwM), SomeException, catches)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative as Opt (Alternative ((<|>)), Parser, execParser, flag, fullDesc, help, helper, info, long, metavar, progDesc, short, showDefault, strOption, value, (<**>))
import Options.Applicative.Common (ParserInfo)
import ProcessRequest (processInteractively, processMany)
import STExcept (SunInfoException (ConfigError))
import System.Exit (ExitCode)
import System.IO.Error (ioeGetFileName, isDoesNotExistError)
import Types (WebAPIAuth)

data AppMode
  = FileInput !FilePath
  | Interactive
  deriving (Show)

data Params
  = Params
      !AppMode --- mode
      !FilePath -- config file
  deriving (Show)

mkParams ∷ Parser Params
mkParams = Params <$> (fileInput <|> interactive) <*> configFile
  where
    fileInput ∷ Parser AppMode
    fileInput = FileInput <$> strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file")
    interactive ∷ Parser AppMode
    interactive = flag Interactive Interactive (long "interactive" <> short 'i' <> help "Interactive mode")
    configFile ∷ Parser String
    configFile = strOption (long "conf" <> short 'c' <> value "config.json" <> showDefault <> metavar "CONFIGNAME" <> help "Configuration file")

withConfig ∷ Params → IO ()
withConfig (Params appMode configFile) = do
  wauth ← eitherDecodeStrict <$> B.readFile configFile
  either (const $ throwM ConfigError) (runMyApp $ run appMode) wauth
  where
    run ∷ AppMode → MyApp ()
    run (FileInput fname) = liftIO (TIO.readFile fname) >>= processMany . T.lines
    run Interactive = processInteractively

-- >>> res ← eitherDecodeStrict <$> B.readFile "./config.json" ∷ IO (Either String WebAPIAuth)
-- >>> res
-- Right (WebAPIAuth {timeZoneDBkey = "AG6C0TV2CAGE", email = "suntimes@chammy.info", agent = "SunTimes.hs"})

main ∷ IO ()
main =
  (execParser opts >>= withConfig)
    `catches` [ Handler parserExit,
                Handler printIOError,
                Handler printOtherErrors
              ]
  where
    opts ∷ ParserInfo Params
    opts = info (mkParams <**> helper) (fullDesc <> progDesc "Reports sunrise/sunset times for the specified location.")
    parserExit ∷ ExitCode → IO ()
    parserExit _ = pure ()
    printIOError ∷ IOException → IO ()
    printIOError e
      | isDoesNotExistError e = do
        let mbfn = ioeGetFileName e
        putStrLn $ "File " <> fromMaybe "" mbfn <> " not found."
      | otherwise = putStrLn $ "I/O error: " <> show e <> "."
    printOtherErrors ∷ SomeException → IO ()
    printOtherErrors = print
