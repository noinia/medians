module Main (main) where

import qualified Data.List as List
import           MediansOfMedians
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do describe "select" $ do
            prop "select same as sorting" $
              \(NonNegative i) (xs :: [Int]) ->
                select i xs === safeSortingSelect i xs
          describe "median" $ do
            prop "median same as sorting" $
              \(xs :: [Int]) ->
                median xs === sortingMedian xs

safeSortingSelect :: Ord a => Int -> [a] -> Maybe a
safeSortingSelect i xs | 0 <= i && i < length xs = Just $ List.sort xs !! i
                       | otherwise               = Nothing


sortingMedian    :: Ord a => [a] -> Maybe (Median a)
sortingMedian xs = let xs' = List.sort xs
                       n   = length xs'
                       h   = (n -1) `div` 2
                   in case xs of
                        []                 -> Nothing
                        [m]                -> Just $ UniqueMedian m
                        _                  -> Just $ if odd n
                                                     then UniqueMedian (xs' !! h)
                                                     else DoubleMedian (xs' !! h) (xs' !! (h+1))
