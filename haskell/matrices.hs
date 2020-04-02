module MatrixOperations where

    type Vector a = [a]
    type Matrix a = [Vector a]

    scalarVector :: Num a => a -> Vector a -> Vector a
    scalarVector n = map (n*)

    addVector :: Num a => Vector a -> Vector a -> Vector a
    addVector = zipWith (+)

    addMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
    addMatrix = zipWith addVector

    getCol :: Matrix a -> Int -> Vector a
    getCol m n = map (!!n) m

    transpose :: Matrix a -> Matrix a
    transpose m = transpAux m 0 len
        where len = length m
    
    transpAux :: Matrix a -> Int -> Int -> [Vector a]
    transpAux _ _ 0 = []
    transpAux m n l = getCol m n : transpAux m n' l'
        where
            n' = n+1
            l' = l-1




    -- sample to try it out
    a :: (Num a) => Vector a
    a = [1, 3, 4]

    b :: (Num a) => Vector a 
    b = [5, 2, 3]

    c :: (Num a) => Vector a
    c = [4, 5, 3]

    mat :: (Num a) => Matrix a 
    mat = [a, b, c]
