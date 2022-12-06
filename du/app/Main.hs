{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (AppConfig (AppConfig), FileOffset, runMyApp)
import Data.Text.IO as TIO (putStr)
import DirTree (dirTree)
import DiskUsage (diskUsage)
import FileCounter (fileCount)
import Options.Applicative as Opt
import TextShow (Builder, TextShow (showb), fromString, toText, unlinesB)

buildEntries ∷ Builder → (e → Builder) → [e] → Builder
buildEntries title entryBuilder entries = unlinesB $ title : map entryBuilder entries

tabEntryBuilder ∷ TextShow s ⇒ (FilePath, s) → Builder
tabEntryBuilder (fp, s) = showb s <> "\t" <> fromString fp

type Depth = Int

treeEntryBuilder ∷ (FilePath, Depth) → Builder
treeEntryBuilder (fp, n) = fromString indent <> fromString fp
  where
    indent = replicate (2 * n) ' '

type FileSize = FileOffset

work ∷ AppConfig → IO ()
work config = do
  dirs ← runMyApp dirTree config ()
  counters ← runMyApp fileCount config ()
  usages ← runMyApp diskUsage config (0 ∷ FileSize)
  let report =
        toText $
          buildEntries "Directory tree:" treeEntryBuilder dirs
            <> buildEntries "\nFile counter:" tabEntryBuilder counters
            <> buildEntries "\nFile space usage:" tabEntryBuilder usages
  TIO.putStr report

mkConfig ∷ Parser AppConfig
mkConfig =
  AppConfig
    <$> strArgument (metavar "DIRECTORY" <> value "." <> showDefault)
    <*> option auto (metavar "DEPTH" <> short 'd' <> long "depth" <> value maxBound <> help "Display an entry for all directories DEPTH directories deep")
    <*> optional (strOption (metavar "EXT" <> short 'e' <> long "extension" <> help "Filter files by extension"))
    <*> switch (short 'L' <> help "Follow symlinks (OFF by default)")

main ∷ IO ()
main = execParser opts >>= work
  where
    opts = info (mkConfig <**> helper) (fullDesc <> progDesc "Directory usage info")
