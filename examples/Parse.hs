{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Parse where

import           Data.Aeson                 (encode, decode)
import           Data.Aeson.TH              (defaultOptions, deriveJSON)
import           Data.ByteString.Lazy.Char8 (putStrLn)
import qualified Data.JSend                 as JSend
import           Prelude                    hiding (putStrLn)

data Person = Person { name :: String
                     , age  :: Int    } deriving (Show)

$(deriveJSON defaultOptions ''Person)

main :: IO ()
main = do

    let john = Person "John" 25

    let text = "{\"status\":\"success\", \"data\":{ \"name\":\"John\", \"age\":25 }}"
    
    -- Note that `Response` expects three type variables:
    --     type of "data" for `Success`
    --     type of "data" for `Fail`
    -- and type of "data" for `Error`.
    -- In simple cases you might use `JSend.Null`.
    let response = decode text :: Maybe (JSend.Response Person JSend.Null JSend.Null)
    
    case response of
      Just (JSend.Success p@(Person _ _)) -> print p
      _ -> putStrLn "Error occured"
      