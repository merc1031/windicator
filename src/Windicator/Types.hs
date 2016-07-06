{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Windicator.Types where

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


