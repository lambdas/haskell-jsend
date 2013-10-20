{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Data.JSend
  ( Response (..)
  , Null
  , jsnull
  , emptySuccess
  , success
  , emptyFail
  , fail
  , emptyError
  , error
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (..), ToJSON (..),
                                      Value (Object, String), object, (.:),
                                      (.:?), (.=))
import qualified Data.Aeson          as Aeson
import           Data.HashMap.Strict (insert, lookup)
import           Data.Text.Lazy      (Text)
import           Prelude             hiding (error, fail, lookup)

default (Text)

data Response s f e = Success { successData  :: s    }
                    | Fail    { failData     :: f    }
                    | Error   { errorData    :: e
                              , errorMessage :: Text }

instance (ToJSON s, ToJSON f, ToJSON e) => ToJSON (Response s f e) where
    toJSON (Success d) = object [ "status"  .= "success"
                                , "data"    .= d         ]
    toJSON (Fail d)    = object [ "status"  .= "fail"
                                , "data"    .= d         ]
    toJSON (Error d m) = object [ "status"  .= "error"
                                , "data"    .= d
                                , "message" .= m         ]

instance (FromJSON s, FromJSON f, FromJSON e) => FromJSON (Response s f e) where
    parseJSON (Object v) =
        case lookup "status" v of
          Just (String "success") -> Success <$> v  .: "data"
          Just (String "fail")    -> Fail    <$> v  .: "data"
          Just (String "error")   -> Error   <$> sv .: "data" <*> v .: "message"
          _                       -> mzero
      -- If `Error` doesn't contain "data", lets pretend that it is `Null`.
      where sv = case lookup "data" v of
                   Nothing -> insert "data" Aeson.Null v
                   _       -> v
    parseJSON _          = mzero

type Null = Maybe Text -- Unfortuantely, Aeson encodes `()` as empty array,
                       -- but we need some dummy type to represent nothing
jsnull :: Null         -- that encodes to "null".
jsnull = Nothing       -- We use `Maybe Text` for it.

emptySuccess :: Response Null Null Null
emptySuccess = Success jsnull

success :: a -> Response a Null Null
success = Success

emptyFail :: Response Null Null Null
emptyFail = Fail jsnull

fail :: a -> Response Null a Null
fail = Fail

emptyError :: Text -> Response Null Null Null
emptyError = Error jsnull

error :: Text -> a -> Response Null Null a
error msg d = Error d msg
