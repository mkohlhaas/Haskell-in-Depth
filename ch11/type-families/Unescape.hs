{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Unescape where

import GHC.Show (showLitChar)
import Unsafe.Coerce (unsafeCoerce)

-- The root of the problem is the Show instance for Char which does all the escaping.
-- >>> "Виталий"
-- "\1042\1080\1090\1072\1083\1080\1081"

-- Let's create our own Char.
newtype UnescapingChar = UnescapingChar {unescapingChar ∷ Char}

-- Create a Show instance for our new Char without any escaping.
-- Just a shameless copy of the Char's Show instance without escaping.
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/src/GHC.Show.html#line-188
-- instance  Show Char  where
--     showsPrec _ '\'' = showString "'\\''"
--     showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''
--     showList cs = showChar '"' . showLitString cs . showChar '"'
instance Show UnescapingChar where
  showsPrec ∷ Int → UnescapingChar → ShowS
  showsPrec _ (UnescapingChar '\'') = showString "'\\''"
  showsPrec _ (UnescapingChar c) = showChar '\'' . showLitChar' c . showChar '\''
  showList ∷ [UnescapingChar] → ShowS
  showList cs = showChar '"' . showLitString' (map unescapingChar cs) . showChar '"'

-- In the original Show instance for Char there is this weird protectEsc function
-- showLitChar c s | c > '\DEL' =  showChar '\\' (protectEsc isDec (shows (ord c)) s)

showLitChar' ∷ Char → ShowS
showLitChar' c s | c > '\DEL' = showChar c s -- here is the main difference to the original Show instance for Char
showLitChar' c s = showLitChar c s

showLitString' ∷ String → ShowS
showLitString' [] s = s
showLitString' ('"' : cs) s = showString "\\\"" (showLitString' cs s)
showLitString' (c : cs) s = showLitChar' c (showLitString' cs s)

-- before
-- >>> 'ë'
-- '\235'
--
-- after
-- >>> UnescapingChar 'ë'
-- 'ë'

-- The problem is that no one uses this nice UnescapingChar and never will.
-- Moreover, we can't teach GHCi to treat the '?' literal as a value of the UnescapingChar type.
-- Here comes an idea: why not transform the type of a given value using type families?
-- We could take any type with Char somewhere inside it and transform it to the same type with Char replaced by UnescapingChar.
-- We'd like to have the following mapping over types:
--   - Char goes to UnescapingChar.
--   - Type constructor will be handled recursively. E.g.
--     - `Maybe Char` becomes `Maybe UnescapingChar`.
--     - [Char] becomes [UnescapingChar] .
--     - `Either String String` becomes `Either [UnescapingChar] [UnescapingChar]`.
--   - All unapplied type constructors (such as Maybe or Either) and concrete types (such as Int or Double) remain unaffected.

type family ToUnescapingTF (a ∷ k) ∷ k where ------------------------ using PolyKinds
  ToUnescapingTF Char = UnescapingChar ------------------------------ Char becomes UnescapingChar
  ToUnescapingTF (t b ∷ k) = (ToUnescapingTF t) (ToUnescapingTF b) -- the applied type constructor needs recursion using currying
  ToUnescapingTF a = a ---------------------------------------------- everything else is left alone

-- >>> :kind! ToUnescapingTF Char
-- ToUnescapingTF Char ∷ Type
-- = UnescapingChar

-- >>> :kind! ToUnescapingTF (Maybe Char)
-- ToUnescapingTF (Maybe Char) ∷ Type
-- = Maybe UnescapingChar

-- >>> :kind! ToUnescapingTF (Either String)
-- ToUnescapingTF (Either String) ∷ Type → Type
-- = Either [UnescapingChar]

-- >>> :kind! ToUnescapingTF (Either String String)
-- ToUnescapingTF (Either String String) ∷ Type
-- = Either [UnescapingChar] [UnescapingChar]

-- >>> :kind! ToUnescapingTF Int
-- ToUnescapingTF Int ∷ Type
-- = Int

-- >>> :kind! ToUnescapingTF [Char]
-- ToUnescapingTF [Char] ∷ Type
-- = [UnescapingChar]

-- >>> :kind! ToUnescapingTF Maybe
-- ToUnescapingTF Maybe ∷ Type → Type
-- = Maybe

-- Now we need a function to process values. As usual, we define it in a type class.
class ToUnescaping a where
  toUnescaping ∷ a → ToUnescapingTF a

-- Due to using newtype for UnescapingChar, the type resulting from `ToUnescapingTF t` shares the same run-time representation as `t`.
-- The unsafeCoerce function implements toUnescaping by doing nothing. Its only role is to persuade the type checker that everything is all right.
instance Show a ⇒ ToUnescaping a where
  toUnescaping ∷ Show a ⇒ a → ToUnescapingTF a
  toUnescaping = unsafeCoerce

-- We can combine these two constraints (ToUnescaping and Show) into one using the ConstraintKinds GHC extension.
type UnescapingShow t = (ToUnescaping t, Show (ToUnescapingTF t))

-- replacement for show
ushow ∷ UnescapingShow t ⇒ t → String
ushow = show . toUnescaping

-- replacement for print
uprint ∷ UnescapingShow t ⇒ t → IO ()
uprint = putStrLn . ushow

-- Works only in GHCi.
-- >>> :set -interactive-print=uprint
-- >>> "Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«"
-- "Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«"

-- Unfortunately, this implementation of the uprint function gives us only a partial solution to the original problem.
-- We use type families to replace Char with UnescapingChar in parameters of type constructors.
-- Parameters to data constructors are invisible for type families.
-- To provide a full solution, one needs to analyze values instead.

-- Following stuff not explained in the book.
mch ∷ ToUnescapingTF (Maybe Char)
mch = toUnescaping (Just 'Ж')

ech ∷ ToUnescapingTF (Either Char Char)
ech = toUnescaping (Left 'Ж' ∷ Either Char Char)

hello ∷ ToUnescapingTF (Maybe String, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char, Char)
hello = toUnescaping (Just "Привет, ", 'м', 'и', 'р', '!', '!', '!', '!', '!', '!', '!', '!', '!')

newtype Name = Name String
  deriving (Show)

name ∷ ToUnescapingTF Name
name = toUnescaping $ Name "Иван"

pangrams ∷ [String]
pangrams =
  [ "Vogt Nyx: »Büß du ja zwölf Qirsch, Kämpe!«",
    "Voix ambiguë d'un cœur qui au zéphyr préfère les jattes de kiwi",
    "Ζαφείρι δέξου πάγκαλο, βαθῶν ψυχῆς τὸ σῆμα",
    "Съешь ещё этих мягких французских булок, да выпей чаю"
  ]
