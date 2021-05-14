module LifeGame
(
  calcNextGen
) where

import qualified Data.Map as Map
import System.IO

calcOverlay :: [Int] -> Map.Map [Int] Int -> Map.Map [Int] Int
calcOverlay key plots = let incRes = Map.mapKeys (zipWith (+) key) plots
                            decRes = Map.mapKeys (zipWith (flip (-)) key) plots
                        in  foldr (Map.unionWith (+)) plots [incRes,decRes]

calcNeighbor :: Map.Map [Int] Int -> Map.Map [Int] Int
calcNeighbor plots = let result = calcOverlay [1,0] $ calcOverlay [0,1] plots
                      in Map.intersectionWith (-) result plots

nextGenCell :: Int -> Int -> Int
nextGenCell cur ngb
  | ngb < 2              = 0
  | cur == 0 && ngb == 3 = 1
  | ngb < 4              = cur
  | otherwise            = 0

calcNextGen :: Map.Map [Int] Int -> Map.Map [Int] Int
calcNextGen plots = Map.unionWith nextGenCell plots $ calcNeighbor plots
