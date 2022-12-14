{-# LANGUAGE RecordWildCards #-}

module RemoteParser where

import Data.Char (isSpace)
import Language.Haskell.Exts (Decl (TypeSig), Name (Ident), ParseResult (ParseOk), SrcSpanInfo, parseDecl)
import Language.Haskell.Meta.Syntax.Translate (toType)
import Language.Haskell.TH as TH

data FuncInfo = FuncInfo
  { name ∷ !String,
    ty ∷ !TH.Type
  }
  deriving (Show)

parseRemoteInterface ∷ String → Q [FuncInfo]
parseRemoteInterface quote = concat <$> mapM (funcInfo . parseDecl) tysigs
  where
    tysigs ∷ [String]
    tysigs = filter (not . null) $ map (dropWhile isSpace) $ lines quote
    funcInfo ∷ ParseResult (Decl SrcSpanInfo) → Q [FuncInfo]
    funcInfo (ParseOk (TypeSig _ ids t)) = pure $ [FuncInfo {..} | let ty = toType t, Ident _ name ← ids]
    funcInfo err = fail $ "Error when parsing remote interface (type signature expected)\n" <> show err
