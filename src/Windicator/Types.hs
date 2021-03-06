{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Windicator.Types where

import Data.Aeson

data SimpleDartType =
      Miss
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Eleven
    | Twelve
    | Thirteen
    | Fourteen
    | Fifteen
    | Sixteen
    | Seventeen
    | Eighteen
    | Nineteen
    | Twenty
    | Bull
    deriving (Enum, Show, Eq, Ord)

data DartNum = Single | Double | Triple
    deriving (Show, Eq, Ord)

instance ToJSON DartNum where
    toJSON dn = String $ go dn
        where go Triple = "TRIPLE"
              go Double = "DOUBLE"
              go Single = "SINGLE_OUTER"

data DartType =
    Simple SimpleDartType DartNum
    deriving (Show, Eq, Ord)

newtype DartChoices = DartChoices Integer deriving (Num, Integral, Real, Enum, Ord, Eq, Show)
newtype Dart = Dart DartType deriving (Ord, Eq, Show)

data WeightedDart = WDart Dart Double deriving (Show, Eq, Ord)

data Config = Config {
      cMiss :: Double
    , cSingle :: Double
    , cDouble :: Double
    , cTriple :: Double
    , cSingleBull :: Double
    , cDoubleBull :: Double
    }

data State = State {
    sHighestValue :: Integer
    }

dartToValue :: SimpleDartType -> Integer
dartToValue n
    | n == Bull = 25
    | otherwise = fromIntegral $ fromEnum n


instance ToJSON WeightedDart where
    toJSON (WDart (Dart (Simple n c)) w) = object $ [
          "type" .= c
        , "value" .= dartToValue n
        , "number" .= fromEnum n
        , "weight" .= w
        ]
