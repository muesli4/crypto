{-# LANGUAGE BangPatterns #-}
module Crypto.GCD where

-- | Uses the advanced euclidian algorithm to compute numbers
-- /(max x y, s, min x y, t)/ for which
-- /gcd (max x y) (min x y) == (max x y) * s + (min x y) * t/.
egcd :: (Ord a, Integral a) => a -> a -> (a, a, a, a)
egcd x y | x < y     = init y x
         | otherwise = init x y
  where
    init a 0 = error "egcd: division by zero" 
    init a b = case divMod a b of { (q, r) -> go a b 1 0 0 1 q r }
      where

        nextV prev curr q = prev - q * curr

        go !i !j !sPrev !s !tPrev !t !q !r
            | r == 0    = (a, s, b, t)
            | otherwise = let sNext    = nextV sPrev s q
                              tNext    = nextV tPrev t q
                              (q', r') = divMod j r
                          in go j r s sNext t tNext q' r'

