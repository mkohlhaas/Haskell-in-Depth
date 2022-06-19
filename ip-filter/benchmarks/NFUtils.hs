{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module NFUtils where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import IPTypes (IP (..), IPRange (..), IPRangeDB (..))

deriving instance Generic IP

deriving instance Generic IPRange

deriving instance Generic IPRangeDB

deriving instance NFData IP

deriving instance NFData IPRange

deriving instance NFData IPRangeDB
