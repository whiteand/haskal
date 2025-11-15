module Haskal.Helper where

trd :: (a, b, c) -> c
trd (_, _, c) = c

listJoin :: [a] -> [[a]] -> [a]
listJoin sep [] = []
listJoin sep [x] = x
listJoin sep (x : xs) = x ++ sep ++ listJoin sep xs