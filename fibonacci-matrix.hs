newtype TBT = TBT ((Integer,Integer),(Integer,Integer)) deriving (Eq,Show)

m :: TBT
m = TBT ((1,1),(1,0))

instance Num TBT where
    fromInteger n = TBT ((n,0),(0,n))
    TBT ((a11,a12),(a21,a22)) * TBT ((b11,b12),(b21,b22)) = TBT ((c11,c12),(c21,c22))
        where
            c11 = a11*b11 + a12*b21
            c12 = a11*b12 + a12*b22
            c21 = a21*b11 + a22*b21
            c22 = a21*b12 + a22*b22

upperLeft :: TBT -> Integer
upperLeft (TBT ((a11,a12),(a21,a22))) = a11

pow :: TBT -> Integer -> TBT
pow _ 0 = fromInteger 1
pow s n
    | odd n = s*r*r
    | otherwise = r*r
        where r = s `pow` (n `div` 2)

--Implementation with naive exponentiation
--linear time
fibSlow :: Integer -> Integer
fibSlow n = upperLeft $ m ^(n-1)

--Implementation using more efficient exponentiation
--logarithmic time
fib :: Integer -> Integer
fib n = upperLeft $ (m `pow` (n-1))
