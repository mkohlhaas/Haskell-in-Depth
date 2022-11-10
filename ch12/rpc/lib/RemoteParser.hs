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

-- >>> parseDecl "ping :: RemotePing PingAnswer"
-- ParseOk (TypeSig (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 1 30, srcInfoPoints = [SrcSpan "<unknown>.hs" 1 6 1 8]}) [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 1 5, srcInfoPoints = []}) "ping"] (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 30, srcInfoPoints = []}) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []}) "RemotePing"))) (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) "PingAnswer")))))

-- pretty printed                                                                                                 variables in funcInfo's pattern matching
-- ParseOk (TypeSig                                                                                                                |
--           (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 1 30, srcInfoPoints = [SrcSpan "<unknown>.hs" 1 6 1 8]})  -- ignored
--           [Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 1 1 5, srcInfoPoints = []}) "ping"]                  -- ids
--           (TyApp (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 30, srcInfoPoints = []})                         -- t
--             (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 9 1 19, srcInfoPoints = []})"RemotePing")))
--             (TyCon (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) (UnQual (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) (Ident (SrcSpanInfo {srcInfoSpan = SrcSpan "<unknown>.hs" 1 20 1 30, srcInfoPoints = []}) "PingAnswer")))))

-- don't use Unicode characters
-- >>> TH.runQ (parseRemoteInterface "ping :: RemotePing PingAnswer\n echo :: String -> RemotePing String")
-- [FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))}]

-- pretty printed
-- [FuncInfo {name = "ping", ty = AppT (ConT RemotePing) (ConT PingAnswer)},
--  FuncInfo {name = "echo", ty = AppT (AppT ArrowT (ConT String)) (AppT (ConT RemotePing) (ConT String))}]
