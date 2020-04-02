{-
   On an 8x8 draughts board, 8 queens must be placed. 
   Knowing that no two queens can share neither column, nor row nor diagonal.
-}

module NQueensSolver
    where
    
    import Data.List (permutations)

    -- given a list of column coordinates, computes possible rows
    coord :: [Int] -> [(Int,Int)]
    coord ns = zip [1.. length ns] ns

    -- returns True if the queen conflicts with the other queen, False otherwise
    conflict :: (Int,Int) -> (Int,Int) -> Bool
    conflict (x0,y0) (x1,y1) =
        ((x0 + y0) == (x1 + y1)) || ((x0 - y0) == (x1 - y1))
   
    -- returns True if queen conflicts with any of the other queens, False otherwise
    conflicts :: (Int,Int) -> [(Int,Int)] -> Bool
    conflicts q@(x,y) qs = any (conflict q) qs

    -- returns True if no queen from the list is in conflict with any other, False otherwise
    isValid :: [(Int,Int)] -> Bool
    isValid [] = True
    isValid [(x,y)] = True
    isValid (xy:xys) = not (conflicts xy xys) && isValid xys

    -- returns the list of possible lists of column coordinates
    solve :: Int -> [[(Int,Int)]]
    solve n = filter isValid $ map coord $ permutations [1..n] 

     -- a more explicit version
    solve' :: Int -> [[(Int,Int)]]
    solve' n = correctAnsw
        where
            allColsPerm     = permutations [1..n] :: [[Int]]
            allPossCoords   = map coord allColsPerm :: [[(Int,Int)]]
            correctAnsw     = filter isValid allPossCoords :: [[(Int,Int)]]