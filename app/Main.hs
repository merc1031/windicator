{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Windicator

main :: IO ()
main = do
    print $ calculate (State 60) 180 3 []
    print $ calculate (State 60) 120 2 []
    print $ calculate (State 60) 60 1 []
    print $ calculate (State 60) 179 3 []

    return ()

