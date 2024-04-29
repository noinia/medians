module MediansOfMedians
  ( median
  , select
  , selectWith
  ) where

import           Data.Functor ((<&>))
import qualified Data.List as List

import           Debug.Trace
--------------------------------------------------------------------------------

data Median a = UniqueMedian !a
              | DoubleMedian !a !a
              deriving (Show,Read,Eq)

-- | Returns the median element
median    :: Ord a => [a] -> Maybe (Median a)
median xs = case xs of
              [] -> Nothing
              _  -> let n = length xs
                    in case n `quotRem` 2 of
                         (h,1) -> UniqueMedian <$> selectWith n (h+1) xs
                         (h,_) -> selectWith n h xs <&> \m ->
                                    let m' = List.minimum $ drop 1 [y | y <- xs, y >= m ]
                                        -- this is safe since: y=m will at least satisfy y
                                        -- >= m moreover, since we computed the lower
                                        -- median, there must also be at least be another element
                                        -- that is ast least m.
                                    in DoubleMedian m m'

-- | returns the i^th smallest element from the list. (The smallest element being the
-- 0^th).
--
-- i.e.
-- prop> select i xs == Just (sort xs !! i)
--
-- \(O(n)\)
select      :: Ord a => Int -> [a] -> Maybe a
select i xs = selectWith (length xs) i xs

-- | Returns the i^th smallest element from the list. (The smallest element being the
-- 0^th). The first argument should be the size of the input list.
--
-- i.e.
-- prop> select i xs == Just (sort xs !! i)
--
-- \(O(n)\)
selectWith        :: Ord a => Int -> Int -> [a] -> Maybe a
selectWith n i xs = case map medianOfFive <$> partitionIntoFives xs of
                      (rest, [])      -> selectSmall i $ List.sort rest
                      (_,    medians) -> case selectWith (n `div` 5) (n `div` 10) medians of
                        Nothing -> error "selectWidth: absurd: cannot happen"
                        -- note: this is safe, since n `div` 10 < n `div` 5 (since n >= 5)
                        Just m  -> let -- m is the median of medians
                          (smallers,eqs,largers) = partitionWith m xs
                          nSmallers              = length smallers
                          nEqs                   = length eqs
                          nLargers               = n - nSmallers - nEqs
                          -- smallers and largers each contain at least a constant fraction
                          -- of the input (of size (3/10)n).
                          i'        = i- nSmallers - nEqs
                          in case i  `compare` nSmallers of
                             LT                         -> selectWith nSmallers i smallers
                             EQ                         -> Just m
                             GT | i <  nSmallers + nEqs -> Just m
                                | otherwise             -> selectWith nLargers i' largers

partitionWith   :: Ord a => a -> [a] -> ([a],[a],[a])
partitionWith m = foldr (\x (smallers,eqs,largers) -> case x `compare` m of
                                                        LT -> (x:smallers,eqs,largers)
                                                        EQ -> (smallers,x:eqs,largers)
                                                        GT -> (smallers,eqs,x:largers)
                        ) ([],[],[])

-- | Just give me the i^th value
selectSmall    :: Int -> [a] -> Maybe a
selectSmall i = \case
  []               -> Nothing
  x:xs | i == 0    -> Just x
       | otherwise -> selectSmall (i-1) xs

-- | Partitions the list into lists of length five
partitionIntoFives :: [a] -> ( [a]   -- ^ the remainder
                             , [[a]] -- ^ the fives
                             )
partitionIntoFives xs = case xs of
  []              -> (xs, [])
  [_]             -> (xs, [])
  [_,_]           -> (xs, [])
  [_,_,_]         -> (xs, [])
  [_,_,_,_]       -> (xs, [])
  (a:b:c:d:e:xs') -> ([a,b,c,d,e]:) <$> partitionIntoFives xs'

-- | pre, input has size exactly five!
medianOfFive                :: Ord a => [a] -> a
medianOfFive xs@[_,_,_,_,_] = List.sort xs !! 2
medianOfFive _              = error "medianOfFive: absurd!"
