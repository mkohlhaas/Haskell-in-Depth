{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import GHC.Generics

data Status = Ok | Err
  deriving (Show, Eq, Generic)

okVal ∷ D1 ('MetaData "Status" "Main" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
okVal = from Ok

-- >>> :info Generic
-- type Generic ∷ Type → Constraint
-- class Generic a where
--   type Rep ∷ Type → Type → Type
--   type family Rep a
--   from ∷ a → Rep a x
--   to ∷ Rep a x → a
--   {-# MINIMAL from, to #-}

-- Not sure how to use Generic1 which can also be derived with the DeriveGeneric extension.
-- >>> :info Generic1
-- type Generic1 ∷ ∀ k. (k → Type) → Constraint
-- class Generic1 f where
--   type Rep1 ∷ ∀ k. (k → Type) → k → Type
--   type family Rep1 f
--   from1 ∷ ∀ (a ∷ k). f a → Rep1 f a
--   to1 ∷ ∀ (a ∷ k). Rep1 f a → f a
--   {-# MINIMAL from1, to1 #-}

-- not very interesting in itself
-- >>> from Ok
-- M1 {unM1 = L1 (M1 {unM1 = U1})}

-- >>> :type from Ok
-- from Ok
--   ∷ D1
--        ('MetaData "Status" "Main" "main" 'False)
--        (C1 ('MetaCons "Ok" 'PrefixI 'False) U1
--         :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1)
--        x

-- >>> :kind! (Rep Status)
-- (Rep Status) ∷ Type → Type
-- = D1
--     ('MetaData "Status" "Main" "main" 'False)
--     (C1 ('MetaCons "Ok" 'PrefixI 'False) U1
--      :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1)

-- >>> Ok == to (from Ok)
-- True

-- >>> :type to (from Ok)
-- to (from Ok)
--   ∷ (Generic a,
--       Rep a
--       ~ M1
--           D
--           ('MetaData "Status" "Main" "main" 'False)
--           (C1 ('MetaCons "Ok" 'PrefixI 'False) U1
--            :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1)) ⇒
--      a

-- >>> to (from Ok) ∷ Status
-- Ok

-- manually building a generic representation
errVal ∷ D1 ('MetaData "Status" "Main" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
errVal = M1 {unM1 = R1 (M1 {unM1 = U1})}

-- >>> from Err
-- M1 {unM1 = R1 (M1 {unM1 = U1})}

-- >>> Err == to errVal
-- True

-- >>> to errVal ∷ Status
-- Err

data Request = Request !String !Int
  deriving (Show, Generic)

-- >>> :kind! Rep Request
-- Rep Request ∷ Type → Type
-- = D1
--     ('MetaData "Request" "Main" "main" 'False)
--     (C1
--        ('MetaCons "Request" 'PrefixI 'False)
--        (S1
--           ('MetaSel
--              'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--           (Rec0 String)
--         :*: S1
--               ('MetaSel
--                  'Nothing 'NoSourceUnpackedness 'SourceStrict 'DecidedStrict)
--               (Rec0 Int)))

-- >>> from $ Request "Get" 10
-- M1 {unM1 = M1 {unM1 = M1 {unM1 = K1 {unK1 = "Get"}} :*: M1 {unM1 = K1 {unK1 = 10}}}}

-- The Generic type class allows us to go from a value to its generic representation and back.
-- >>> to (from $ Request "Get" 10) ∷ Request
-- Request "Get" 10

main ∷ IO ()
main = do
  print $ from Ok ----------- M1 {unM1 = L1 (M1 {unM1 = U1})}
  print $ from Err ---------- M1 {unM1 = R1 (M1 {unM1 = U1})}
  print $ Err == to errVal -- True
