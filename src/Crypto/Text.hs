module Crypto.Text where

import Data.Monoid
import Data.Foldable as F
import Data.Vector   as V
import Prelude       as P

import Crypto

-- | All subvectors in the vector of a specific length which could reappear.
-- Provides constant time lookup for subvectors of the current length at every
-- position. /O(n)/
repVecs :: Int -> Vector c -> Vector (Vector c)
repVecs l v = generate (V.length v - l + 1) (\i -> slice i l v)

-- | Finds matches of a given length in the prepared lookup vector. /O(n^2)/
findMatches :: Eq c => Int -> Vector (Vector c) -> [(Vector c, Int, Int)]
findMatches l ps = P.concat $ P.zipWith f [0 ..] $ V.toList $ ps
  where
    f i v = [ (v, i, j - i) | j <- [i + l .. V.length ps - l], ps ! j == v ]

repetitions :: Eq c => Int -> Vector c -> [(Vector c, Int, Int)]
repetitions l = findMatches l . repVecs l

-- | Removes all submatches, requires the bigger matches to appear first.
pruneSubMatches :: [(Vector c, Int, Int)] -> [(Vector c, Int, Int)]
pruneSubMatches = go []
  where
    go bl (m : ms) | (m `hits`) `P.any` bl = go bl ms
                   | otherwise             = go (m : bl) ms
    go bl []                               = bl

    hits (lv, li, lo) (rv, ri, ro) = let ll = V.length lv   
                                         rl = V.length rv
                                     in ll < rl && ri <= li && ri + rl >= li + ll && lo == ro

-- | Splits the input into n pieces, whereas n is a positive non-zero number.
multiplex :: Int -> Vector c -> [Vector c]
multiplex n v = fmap f [0 .. n - 1]
  where
    f o = generate (max - (if r == 0 || r >= o + 1 then 0 else 1)) (\i -> v ! (i * n + o))
    vl  = V.length v
    max = ceiling $ fromIntegral vl / fromIntegral n
    r   = vl `rem` n

-- | Find all repetitions starting with a length of 3 up to n / 2.
kasiski :: Eq c => V.Vector c -> [(V.Vector c, Int, Int, [Int])]
kasiski v = fmap addFactors $ pruneSubMatches reps
  where
    reps                  = P.concat [ repetitions l v | l <- [m, m - 1 .. 3] ]
    m                     = V.length v `quot` 2
    addFactors (v, p, os) = (v, p, os, factors os)

coincidenceIndex :: (F.Foldable f, Fractional n) => f n -> n -> n
coincidenceIndex lis l = getSum (F.foldMap (\li -> Sum $ li * (li - 1)) lis) / (l * (l - 1))

friedman :: (F.Foldable f, Fractional n) => f n -> n -> n
friedman lis l = 0.0377 * l / ((l - 1) * coincidenceIndex lis l - 0.0385 * l + 0.0762)

fromGerOrd :: Int -> Char
toGerOrd :: Char -> Int
(fromGerOrd, toGerOrd) = (toEnum . (+ aOrd) . flip mod 26, subtract aOrd . fromEnum)
  where
    aOrd = fromEnum 'A'

-- | Decrypt a cypher text which is encoded with the Vigenère encryption using
-- a specific keyword, given as list of distances.
decryptVigenère :: [Int] -> String -> String
decryptVigenère ks = P.zipWith (\k c -> fromGerOrd $ toGerOrd c - k) $ cycle ks

