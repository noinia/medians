module Main (main) where

import qualified Data.List as List
import           MyLib
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = do let xs = [0,0,0,-1,-1]
                -- [-1,-1,0] -- [1,43,32,5,4,352,52,54,1,2,100,343]
              n  = length xs
          print n
          print $ List.sort xs !! 2
          print $ selectWith n 2 xs
          -- print $ median xs
          -- print $ partitionIntoFives xs
          hspec spec

spec = do describe "select" $ do
            prop "select same as sorting" $
              \(NonNegative i) (xs :: [Int]) ->
                select i xs === safeSortingSelect i xs


safeSortingSelect i xs | 0 <= i && i < length xs = Just $ List.sort xs !! i
                       | otherwise               = Nothing
