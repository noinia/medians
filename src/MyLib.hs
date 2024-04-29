module MyLib
  ( median
  , select
  , selectWith
  ) where

import qualified Data.List as List

import Debug.Trace
--------------------------------------------------------------------------------

data Median a = UniqueMedian !a
              | DoubleMedian !a !a
              deriving (Show,Read,Eq)

-- | Returns the median element
median    :: (Ord a, Show a) => [a] -> Maybe (Median a)
median xs = case xs of
              [] -> Nothing
              _  -> let n = length xs
                    in case n `quotRem` 2 of
                         (h,1) -> UniqueMedian <$> selectWith n (h+1) xs
                         (h,_) -> let Just m  = selectWith n h xs
                                      m' = List.minimum $ tail [y | y <- xs, y >= m ]
                                      -- this is safe since: y=m will at least satisfy y
                                      -- >= m moreover, since we computed the lower
                                      -- median, there must also be at least be another element
                                      -- that is ast least m.
                                  in Just $ DoubleMedian m m'

-- | returns the i^th smallest element from the list. (The smallest element being the
-- 0^th).
--
-- i.e.
-- prop> select i xs == Just (sort xs !! i)
--
-- \(O(n)\)
select      :: (Ord a, Show a) => Int -> [a] -> Maybe a
select i xs = selectWith (length xs) i xs

selectWith        :: (Ord a, Show a) => Int -> Int -> [a] -> Maybe a

selectWith n i xs = case map medianOfFive <$> partitionIntoFives xs of
                      (rest, [])      -> selectSmall i $ List.sort rest
                      (rest, medians) ->
                        let Just m    = selectWith (n `div` 5) (n `div` 10) medians
                            -- note: this is safe, since n `div` 10 < n `div` 5 (since n >= 5)
                            (smallers,eqs,largers) = partitionWith m xs
                            nSmallers = length smallers
                            nEqs      = length eqs
                            nLargers  = n - nSmallers - nEqs
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

-- maximum' :: Ord a => [a] -> Maybe a
-- maximum' = \case
--   []   -> Nothing
--   x:xs -> Just $ List.foldl' max x xs

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
  [_]             -> (xs, [])
  [_,_]           -> (xs, [])
  [_,_,_]         -> (xs, [])
  [_,_,_,_]       -> (xs, [])
  (a:b:c:d:e:xs') -> ([a,b,c,d,e]:) <$> partitionIntoFives xs'

-- | pre, input has size exactly five!
medianOfFive                :: Ord a => [a] -> a
medianOfFive xs@[_,_,_,_,_] = List.sort xs !! 2
medianOfFive _              = error "medianOfFive: absurd!"
