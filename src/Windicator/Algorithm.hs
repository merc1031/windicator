{-# LANGUAGE BangPatterns #-}
module Windicator.Algorithm where

import Debug.Trace
import Data.List (sort, nub, sortOn, group)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Control.Arrow ((&&&))

import Windicator.Types

perValueConfig :: Config
perValueConfig = Config 0.9 0.9 0.4 0.5 0.4 0.3

perSetConfig :: Config
perSetConfig = Config 0.0 0.0 0.0 0.0 0.0 0.0

smear :: SimpleDartType -> Double
smear dval =
    let value = (fromEnum dval)
        calc = ((fromIntegral $ value - 1) / (21 - 1)) / 10
    in if value >= 15
          then calc + calc
          else calc

dropBadValues :: [[WeightedDart]] -> [[WeightedDart]]
dropBadValues = map (filter (\(WDart (Dart (Simple typ _)) _) -> typ /= Miss))

getWeights :: Dart -> Config -> Double
getWeights (Dart d) cfg = go d
    where go (Simple Miss _) = cMiss cfg
          go (Simple Bull Single) = cSingleBull cfg
          go (Simple Bull Double) = cDoubleBull cfg
          go (Simple dval Triple) = cTriple cfg + smear dval
          go (Simple dval Double) = cDouble cfg + smear dval
          go (Simple dval Single) = cSingle cfg + smear dval

reWeigh :: [[Dart]] -> [[WeightedDart]]
reWeigh choices =
    let perValueWeigher val = WDart val $ getWeights val perValueConfig
        perSetWeigher vals =
            let countList = map (length &&& head) $ group $ sort vals
                w (c, (WDart d _)) =
                    let weight = getWeights d perSetConfig
                    in weight * fromIntegral c
            in foldr (*) 1 $ map w countList
        choicesWithWeights = map (map perValueWeigher) choices
    in reverse $ flip sortOn choicesWithWeights $ (\throws ->
            let pW = perSetWeigher throws
                vW = foldr (\(WDart _ w) acc -> acc * w) 1 throws
            in pW + vW
            )

simpleDartFromInt :: Integer -> SimpleDartType
simpleDartFromInt v = toEnum $ fromIntegral v

dartFromValue :: Integer -> SimpleDartType
dartFromValue val = go val
    where go val
            | val == 25 = Bull
            | val == 0 = Miss
            | otherwise = simpleDartFromInt val

expandDarts :: [[Integer]] -> [[Dart]]
expandDarts choices =
    let genDartsForValues vals =
            let genDartForValue :: Integer -> [Dart]
                genDartForValue val =
                    let three = if (val `mod` 3 == 0)
                            then Just $ Dart $ Simple (dartFromValue (val `div` 3)) Triple
                            else Nothing
                        two = if ((val <= 40 || val == 50) && (val `mod` 2 == 0))
                            then Just $ Dart $ Simple (dartFromValue (val `div` 2)) Double
                            else Nothing
                        one = if (val <= 20 || val == 25)
                            then Just $ Dart $ Simple (dartFromValue val) Single
                            else Nothing
                    in case val of
                        0 -> [Dart $ Simple Miss Single]
                        _ -> catMaybes [three, two, one]
            in sequence (map genDartForValue vals)
    in concat $ map genDartsForValues choices

findCombinationsForTarget :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [[a]]
findCombinationsForTarget !_ [] acc = acc
findCombinationsForTarget !n (x:xs) acc =
    let matches = [x | (== n) $ sum x]
    in findCombinationsForTarget n xs $ concat [matches, acc]


combsWithRep' :: (Ord a, Integral b) => b -> [a] -> [[a]]
combsWithRep' n xs =
    let combinations = combsWithRep n xs
    in map (reverse . sort) combinations

combsWithRep :: Integral b => b -> [a] -> [[a]]
combsWithRep !0  _ = [[]]
combsWithRep !_ [] = []
combsWithRep !n xs@(y:ys) =
    let all = map (y:) (combsWithRep (n-1) xs) <>  rest
        rest = combsWithRep n ys
    in all


calculate :: State -> Integer -> Integer -> [Dart] -> [[WeightedDart]]
calculate (State highest) goal throws _
    | highest * throws >= goal = dropBadValues (reWeigh $ expandDarts $ findCombinationsForTarget goal (combsWithRep' throws $ filter (<= goal) generate ) [] )
    | otherwise = []

generate :: [Integer]
generate = let miss = 0
               values = [1..20]
               bulls = [25,50]
               allValues = sort $ nub $ ((miss:values) <> (map (*2) values) <> (map (*3) values) <> [25,50])
           in allValues
