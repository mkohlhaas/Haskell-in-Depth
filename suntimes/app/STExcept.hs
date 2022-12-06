{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}

module STExcept where

import Control.Monad.Catch (Exception (toException), MonadThrow (..), SomeException)
import Data.Text (Text)
import qualified Network.HTTP.Client as NC
import Network.HTTP.Req (HttpException (..))
import Types (GeoCoords)

data RequestError = EmptyRequest | WrongDay !Text
  deriving (Show)

data SunInfoException
  = UnknownLocation !Text
  | UnknownTime !GeoCoords
  | FormatError !RequestError
  | ServiceAPIError !String
  | NetworkError !SomeException
  | ConfigError
  deriving (Exception)

instance Show SunInfoException where
  show ∷ SunInfoException → String
  show (UnknownLocation _) = "Failed while determining coordinates"
  show (UnknownTime _) = "Failed while determining sunrise/sunset times"
  show (FormatError er) = show er
  show (ServiceAPIError _) = "Error while communicating with external services"
  show (NetworkError _) = "Network communication error"
  show ConfigError = "Error parsing configuration file"

-- HttpException is from the Network.HTTP.Client which is used by the `req` package
rethrowReqException ∷ MonadThrow m ⇒ HttpException → m a
rethrowReqException (JsonHttpException s) = throwM (ServiceAPIError s)
rethrowReqException (VanillaHttpException (NC.HttpExceptionRequest _ (NC.StatusCodeException resp _))) = throwM (ServiceAPIError $ show $ NC.responseStatus resp)
rethrowReqException (VanillaHttpException e) = throwM (NetworkError $ toException e)
