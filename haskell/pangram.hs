{-
Exercism's pangram problem:
Determine if a sentence is a pangram. Aka a sentence using every letter of the alphabet at least once. The best known English pangram is:
-}

module Pangram (isPangram) where

import qualified Data.Char as DC
import Control.Monad (liftM2)
import Data.Set (fromList, size)

alph :: String
alph = ['a'..'z']

isPangram :: String -> Bool
isPangram t = length alph == size  (fromList $ filtering $ map DC.toLower t)

filtering :: String -> String
filtering = filter $ liftM2 (&&) DC.isAscii DC.isAlpha
