module CollatzConjecture (collatz) where

{-Computes the number of steps taken from the input to termination -}

collatz :: Integer -> Maybe Integer
collatz n
        | n <= 0    = Nothing
        | otherwise = Just $ collatzAux 0 n


collatzAux :: Integer -> Integer -> Integer
collatzAux counter n 
        | n == 1        = counter
        | r == 0        = collatzAux newCounter q
        | otherwise     = collatzAux  newCounter $ n *3 +1
    where
        newCounter = counter +1
        (q,r)      = n `quotRem` 2
        
