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
      