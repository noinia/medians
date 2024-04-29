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

safeSortingSelect :: Ord a => Int -> [a] -> Maybe a
safeSortingSelect i xs | 0 <= i && i < length xs = Just $ List.sort xs !! i
                       | otherwise               = Nothing
