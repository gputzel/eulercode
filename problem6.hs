problem6 = (sum [1..100])^2 - sum [n^2 | n <- [1..100]]

problem6better :: Integer -> Integer
problem6better n = squareOfSum - sumOfSquares
    where
        sumOfSquares = n*(n+1)*(2*n+1) `div` 6
        squareOfSum = theSum^2
        theSum = n*(n+1) `div` 2
