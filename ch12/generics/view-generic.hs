{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import GHC.Generics (C1, D1, FixityI (PrefixI), Generic (from, to), M1 (M1, unM1), Meta (MetaCons, MetaData), U1 (..), type (:+:) (R1))

data Status = Ok | Err
  deriving (Show, Eq, Generic)

data Request = Request String Int
  deriving (Generic)

okVal = from Ok

errVal :: D1 ('MetaData "Status" "Main" "main" 'False) (C1 ('MetaCons "Ok" 'PrefixI 'False) U1 :+: C1 ('MetaCons "Err" 'PrefixI 'False) U1) x
errVal = M1 {unM1 = R1 (M1 {unM1 = U1})}

main :: IO ()
main = do
  print $ from Ok
  print $ from Err
  print $ Err == to errVal
