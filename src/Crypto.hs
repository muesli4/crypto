module Crypto where

import           Control.Arrow
import qualified Data.Foldable as F
import           Data.Function
import           Data.List
import qualified Data.Map      as M

-- | Creates a histogram.
hist :: (F.Foldable f, Ord a, Num n) => f a -> M.Map a n
hist = F.foldr (M.alter $ Just . maybe 1 (+ 1)) M.empty

histL :: (F.Foldable f, Ord a, Num n) => f a -> [(a, n)]
histL = M.toList . hist

probL :: (F.Foldable f, Ord a, Fractional n) => n -> f a -> [(a, n)]
probL l = map (second (/ l)) . histL

sortBySndDesc :: Ord b => [(a, b)] -> [(a, b)]
sortBySndDesc = sortBy (flip compare `on` snd)

-- | Computes all factors to a given positive number (excluding 1).
factors :: Integral i => i -> [i]
factors n = go n 2
  where
    max                     = n `quot` 2

    go 1 _                  = []
    go m d | d > max        = [m]
           | m `rem` d == 0 = d : go (m `quot` d) d
           | otherwise      = go m (d + 1)

prime :: Integral i => i -> Bool
prime n = factors n == [n]

coprime :: Integral i => i -> i -> Bool
coprime x y = gcd x y == 1

totatives :: Integral i => i -> [i]
totatives n | n < 2     = [1]
            | otherwise = [ t | t <- [1 .. n - 1], coprime t n ]

totient :: Integral i => i -> i
totient = fromIntegral . length . totatives

