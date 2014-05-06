module Utils

where

import System.Random
import Data.List

type Objective = Double
type Point     = [Objective]
type Points    = [Point]
type Volume    = Objective
type Stack     = [(Point, Int)]

------------------------------------------------------- I/O stuff

printPoints :: [Points] -> String
-- printPoints pss returns the file contents for pss
printPoints = unlines . foldr f [separator]
              where f ps ls = separator : map (unwords . map show) ps ++ ls

separator :: String
-- separator returns the line separator used in data files
separator = "#"

parsePoints :: String -> [Points]
-- parsePoints s returns the sets of points in s
parsePoints = parsePoints' . tail . lines
         where parsePoints' [] = []
               parsePoints' ls = map (map read . words) xs : parsePoints' ys
                                 where (xs, _ : ys) = break (== separator) ls

------------------------------------------------------- domination and sorting

dominates :: Ordering -> Point -> Point -> Bool
-- pre: length p == length p'
-- dominates d p p' returns True iff p dominates p' in an optimisation problem towards d
-- complexity: length p
dominates d = dom False
              where dom b (x : xs) (y : ys) = dom' (compare x y) b xs ys
                    dom b []       []       = b
                    dom' z b xs ys | z == d    = dom True xs ys
                                   | z == EQ   = dom b    xs ys
                                   | otherwise = False

order :: Ord a => Bool -> a -> a -> Bool
order  True  = (>=) -- True  is maximisation
order  False = (<=) -- False is minimisation

better :: Bool -> Ordering
better True  = GT
better False = LT

insertin :: Bool -> Point -> Points -> Points
-- pre: xs is sorted by p
-- insertin p x xs returns x : xs sorted by order p,
-- with points dominated by x discarded
insertin _p x []            = [x]
insertin p x zs @ (y : ys) | order p y x = y : insertin p x ys
                           | otherwise   = x : filter (not . dominates (better p) x) zs

qsort :: (a -> a -> Bool) -> [a] -> [a]
-- qsort f xs returns xs sorted according to f
qsort _f []       = []
qsort f (x : xs) = qsort f ys ++ [x] ++ qsort f zs
                   where (ys, zs) = partition (`f` x) xs

------------------------------------------------------- basic stuff

allsubsets, nonemptysubsets :: [a] -> [[a]]
-- allsubsets xs returns a list of all subsets of xs
allsubsets []       = [[]]
allsubsets (x : xs) = [zs | ys <- allsubsets xs, zs <- [x : ys, ys]]

-- nonemptysubsets xs returns a list of non-empty subsets of xs
nonemptysubsets = filter (not . null) . allsubsets

perms :: [a] -> [[a]]
-- perms xs returns all permutations of xs
perms []       = [[]]
perms (x : xs) = [take k ys ++ [x] ++ drop k ys 
                 | ys <- perms xs, k <- [0 .. length xs]]

permColumns :: [[a]] -> [[[a]]]
-- permColumns xss returns all permutations of the columns of xss
permColumns = map transpose . perms . transpose

seeds :: StdGen -> [StdGen]
-- seeds g returns an infinite list of random seeds derived from g
seeds g = g' : seeds g''
          where (g', g'') = split g

foldlseq :: (b -> a -> b) -> b -> [a] -> b
foldlseq _f b []       = b
foldlseq f b (x : xs) = z `seq` foldlseq f z xs
                        where z = f b x

foldlseq' :: ((b, c) -> a -> (b, c)) -> (b, c) -> [a] -> (b, c)
foldlseq' _f b []       = b
foldlseq' f b (x : xs) = fst z `seq` snd z `seq` foldlseq' f z xs
                         where z = f b x
