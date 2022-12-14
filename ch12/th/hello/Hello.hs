{-# LANGUAGE TemplateHaskell #-}

module Hello where

import Language.Haskell.TH

-- Better visit these web pages for an introduction to TH!
-- - `https://serokell.io/blog/introduction-to-template-haskell`
--   - `https://github.com/mkohlhaas/Serokell-Introduction-to-Template-Haskell`

-- ExpQ(= Q Expr): [| 42 |]
-- Exp/AST:        LitE (IntegerL 42)
--
--        ExpQ              Exp
--          |                |
-- runQ [| 42 |] = m LitE (IntegerL 42)
--                 |
--         in a mondadic context
--
-- Splicing $(...) needs an ExpQ and "evaluates" it, i.e. generates code.
-- [| ... |] are called Oxford brackets.
-- >>> $([| 42 |])
-- 42

-- It's the inverse of the Oxford brackets.
-- Splicing is like copy and paste of source code.

-- Besides expressions (the Exp type), Haskell ASTs may contain declarations (Dec), types (Type), and patterns (Pat).
-- We can construct and splice all of them.

-- [A practical Template Haskell Tutorial](https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial)
-- Quotation brackets quote regular Haskell code as the corresponding object program fragments inside the Q monad.
-- There are quotation brackets for quoting Haskell expressions ([e| .. |]|), patterns ([p| .. |]), declarations ([d| .. |]), and types ([t| .. |]).
-- Writing [| .. |] is hereby just another way of saying [e| .. |].
--
-- Using quotation brackets in a sense lifts Haskell's concrete syntax into corresponding object program expressions (AST) inside the Q monad.
-- By doing so, quotation brackets represent the dual of the splice operator $.
-- Evaluating a meta program with "$" splices in the generated object program as real Haskell code.
-- In contrast, quotation brackets [| .. |] turn real Haskell code into an object program.
-- Consequently, quotation brackets and the splice operator cancel each other out.
-- The equation $([| e |]) = e holds for all expressions e and similar equations hold for declarations, and types.

-- IO context
-- >>> runQ [| 42 |]
-- LitE (IntegerL 42)

-- IO context
-- >>> $(pure $ LitE (IntegerL 42))
-- 42

-- >>> runQ [| 1 + 2 |]
-- InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))

-- runQ runs computation in the Q monad
-- >>> :type runQ
-- runQ ∷ Quasi m ⇒ Q a → m a

-- Constructing an ExpQ by hand.
-- Note the usage of mkName!
-- >>> InfixE (Just (LitE (IntegerL 1))) (VarE (mkName "+")) (Just (LitE (IntegerL 2)))
-- InfixE (Just (LitE (IntegerL 1))) (VarE +) (Just (LitE (IntegerL 2)))

-- >>> :type mkName
-- mkName ∷ String → Name

-- >>> $(pure $ InfixE (Just (LitE (IntegerL 1))) (VarE (mkName "+")) (Just (LitE (IntegerL 2))))
-- 3

hello ∷ ExpQ
hello = [|putStrLn "Hello world"|]

-- >>> runQ hello
-- AppE (VarE System.IO.putStrLn) (LitE (StringL "Hello world"))

hello1 ∷ ExpQ
hello1 = [|"Hello world"|]

-- Staging
-- It is not allowed to splice TH definitions created in the same module.
-- GHC error: ‘hello1’ is used in a top-level splice, quasi-quote, or annotation, and must be imported, not defined locally.
-- $hello1

-- >>> runQ hello1
-- LitE (StringL "Hello world")

-- >>> $(hello1)
-- "Hello world"

-- Parentheses not needed single name splices.
-- >>> $hello1

-- >>> $([| "Hello world" |])
-- "Hello world"
