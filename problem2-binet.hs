-- (S5N) a b represents a + b*sqrt(5)
data S5N = S5N Integer Integer deriving Show

--By analogy with complex numbers, the "imaginary" part
--of a + b*sqrt(5) is b
ipart :: S5N -> Integer
ipart (S5N a b) = b

--By analogy with complex numbers, the "real" part 
--of a + b*sqrt(5) is a
rpart :: S5N -> Integer
rpart (S5N a b) = a

--Define operations of multiplication and subtraction
--(a + b*sqrt(5)) * (c + d*sqrt(5))
mult :: S5N -> S5N -> S5N
(S5N a b) `mult` (S5N c d) = S5N (a*c + 5*b*d) (a*d + b*c)

plus :: S5N -> S5N -> S5N
(S5N a b) `plus` (S5N c d) = S5N (a + c) (b + d)

minus :: S5N -> S5N -> S5N
(S5N a b) `minus` (S5N c d) = S5N (a - c) (b - d)

--Naive implementation of exponentiation
--Linear in n
powSlow :: S5N -> Integer -> S5N
powSlow _ 0 = (S5N 1 0)
powSlow s n = s `mult` (powSlow s (n-1))

--Faster implementation of exponentiation
--Relies on binary expansion of n
--Logarithmic in n
pow :: S5N -> Integer -> S5N
pow _ 0 = (S5N 1 0)
pow s n
    | odd n = s `mult` (root `mult` root)
    | otherwise = (root `mult` root)
        where root = (s `pow` (n `div` 2))

--Well, actually 2*phi and 2*psi
--We will divide by the power of 2 at the end
phi = S5N 1 1
psi = S5N 1 (-1)

--Find the nth Fibonacci number usng the Binet formula
--Uses slow (linear time) exponentiation
fibSlow :: Integer -> Integer
fibSlow n = (ipart difference) `div` (2^n)
            where difference = (phi `powSlow` n) `minus` (psi `powSlow` n)

--Find the nth Fibonacci number usng the Binet formula
--Uses faster (logarithmic time) exponentation
fib :: Integer -> Integer
fib n = (ipart difference) `div` (2^n)
            where difference = (phi `pow` n) `minus` (psi `pow` n)
