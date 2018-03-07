import Data.Ratio

-- (S5N) a b represents a + b*sqrt(5)
newtype S5N a = S5N (a,a) deriving (Eq,Show)

instance (Num a) => Num (S5N a) where
    S5N (a,b) + S5N (c,d) = S5N (a+c,b+d)
    S5N (a,b) - S5N (c,d) = S5N (a-c,b-d)
    S5N (a,b) * S5N (c,d) = S5N (a*c + 5*b*d, a*d + b*c)
    fromInteger i = S5N (fromInteger i,0)

--By analogy with complex numbers, the "imaginary" part
--of m + n*sqrt(5) is b
ipart :: (S5N a) -> a
ipart (S5N (m,n)) = n

--By analogy with complex numbers, the "real" part 
--of m + n*sqrt(5) is a
rpart :: (S5N a) -> a
rpart (S5N (m,n)) = m

--Naive implementation of exponentiation
--Linear in n
powSlow :: Num a => (S5N a) -> Integer -> (S5N a)
powSlow _ 0 = fromInteger 1
powSlow s n = s * (powSlow s (n-1))

--Faster implementation of exponentiation
--Relies on binary expansion of n
--Logarithmic in n
pow :: Num a => (S5N a) -> Integer -> (S5N a)
pow _ 0 = fromInteger 1
pow s n
    | odd n = s * (root * root)
    | otherwise = (root * root)
        where root = (s `pow` (n `div` 2))

phi = S5N (1 % 2, 1 % 2)
psi = S5N (1 % 2, (-1) % 2)

--Find the nth Fibonacci number usng the Binet formula
--Uses slow (linear time) exponentiation
fibSlow :: Integer -> Integer
fibSlow n = numerator (ipart difference)
            where difference = (phi `powSlow` n) - (psi `powSlow` n)

--Find the nth Fibonacci number usng the Binet formula
--Uses faster (logarithmic time) exponentation
fib :: Integer -> Integer
fib n = numerator (ipart difference)
            where difference = (phi `pow` n) - (psi `pow` n)
