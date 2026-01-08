module Haskal.Helper where

import Data.List (intercalate)

trd :: (a, b, c) -> c
trd (_, _, c) = c