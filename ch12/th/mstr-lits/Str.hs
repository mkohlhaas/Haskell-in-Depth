-- We can define our own Oxford bracket processors, called quasiquoters.
-- A quasiquoter is a value of the QuasiQuoter type defined in the Language.Haskell.TH.Quote module.
-- This value should contain the following four functions as its fields for parsing quotes found in different contexts:
-- - expression
-- - pattern
-- - type
-- - top-level declaration
-- These functions should return the corresponding TH data types in the Q monad.
-- Whenever GHC meets a quasiquotation, it calls the corresponding function and splices its result back to the code.

module Str where

import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteDec, quoteExp, quotePat, quoteType))

-- >>> :type stringE
-- stringE ∷ String → ExpQ

str ∷ QuasiQuoter
str =
  QuasiQuoter
    { quoteExp = stringE, ----- In general, we have to implement parsing of quote content by ourselves.
      quotePat = undefined, --- we don’t expect multiline String literals in a pattern     context
      quoteType = undefined, -- we don’t expect multiline String literals in a type        context
      quoteDec = undefined ---- we don’t expect multiline String literals in a declaration context
    }
