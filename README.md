# Haskell JSend

Simple [JSend](http://labs.omniti.com/labs/jsend) implementation in Haskell.


# Usage

JSend response is represented by `Response` type that can be serialized/deserialized using [Aeson](https://github.com/bos/aeson):

```haskell
data Response a = Success { responseData    :: Maybe a }
                | Fail    { responseData    :: Maybe a }
                | Error   { responseData    :: Maybe a
                          , responseMessage :: Text    }
```

You can construct `Response` manually, but there are several helpers:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Encode where

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

    -- Empty success
    -- { "status":"success", "data":null }
    putStrLn . encode $ JSend.emptySuccess

    -- Success with data
    -- { "status":"success", "data":{ "name":"John", "age":25 } }
    putStrLn . encode $ JSend.success john

    -- Empty fail
    -- { "status":"fail", "data":null }
    putStrLn . encode $ JSend.emptyFail

    -- Fail with data
    -- { "status":"fail", "data":{ "name":"John", "age":25 } }
    putStrLn . encode $ JSend.fail john

    -- Empty error
    -- { "status":"error", "message":"Error occured", "data":null }
    putStrLn . encode $ JSend.emptyError "Something goes wrong"

    -- Error with data
    -- { "status":"error", "message":"Error occured",
    --   "data":{ "name":"John","age":25 } }
    putStrLn . encode $ JSend.error "Something goes wrong" john
```

Parsing is a little bit trickier, because `Response` expects three type variables:
+ type of "data" for `Success` case
+ type of "data" for `Fail` case
+ type of "data" for `Error` case

In simple cases you might use `JSend.Null`.

```haskell
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
    let response = decode text :: Maybe (JSend.Response Person JSend.Null JSend.Null)

    case response of
      Just (JSend.Success p@(Person _ _)) -> print p
      _ -> putStrLn "Error occured"
```
