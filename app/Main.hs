{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Windicator
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Aeson.Encode.Pretty (encodePretty)

printJSON = BSLC.putStrLn . encodePretty

main :: IO ()
main = do
    printJSON $ calculate (State 60) 180 3 []
    printJSON $ calculate (State 60) 120 2 []
    printJSON $ calculate (State 60) 60 1 []
    printJSON $ calculate (State 60) 179 3 []

    return ()

