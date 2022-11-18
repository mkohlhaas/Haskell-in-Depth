module FilmInfoFromField where

import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple.FromField
import FilmInfoData

instance FromField Rating where
  fromField f Nothing = returnError UnexpectedNull f ""
  fromField f (Just bs) =
    case toMaybeRating bs of
      Nothing → returnError ConversionFailed f (B.unpack bs)
      Just r → pure r
