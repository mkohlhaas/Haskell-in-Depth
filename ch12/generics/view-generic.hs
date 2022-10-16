{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

import GHC.Generics
  ( C1,
    D1,
    FixityI (PrefixI),
    Generic (from, to),
    M1 (M1, unM1),
    Meta (MetaCons, MetaData),
    Rep,
    U1 (..),
    type (:+:) (R1),
  )

data Status = Ok | Err
  deriving (Show, Eq, Generic)

okVal ∷ D1 ('MetaData "Status" "Main" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
okVal = from Ok

errVal ∷ D1 ('MetaData "Status" "Main" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
errVal = M1 {unM1 = R1 (M1 {unM1 = U1})}

-- >>> from Ok
-- M1 {unM1 = L1 (M1 {unM1 = U1})}

-- >>> Ok == to okVal
-- True

-- >>> to okVal ∷ Status
-- Ok

-- >>> from Err
-- M1 {unM1 = R1 (M1 {unM1 = U1})}

-- >>> Err == to errVal
-- True

-- >>> to errVal ∷ Status
-- Err

data Request = Request !String !Int
  deriving (Show, Generic)

-- >>> :kind! Rep Request
-- Rep Request :: Type -> Type
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
  print $ from Ok
  print $ from Err
  print $ Err == to errVal
