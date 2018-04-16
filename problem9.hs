isSquare :: Int -> Bool
isSquare n = truncate(sqrt(x)) * truncate(sqrt(x)) == n
             where x = fromIntegral n

problem9 = [a*b*(truncate $ sqrt $ fromIntegral (b^2-a^2)) | b <- [1..1000], a <- [1..b-1],isSquare (b^2-a^2),a + b + (truncate $ sqrt $ fromIntegral (b^2-a^2)) == 1000 ]

--From gauchopuro, much simpler
problem9better = product . head $ [[a, b, c] |
      a <- [1..1000], b <- [1..1000], c <- [1000 - a - b],
      a ^ 2 + b ^ 2 == c ^ 2]

problem9general :: Integer -> [[Integer]]
problem9general n = [[a, b, c] |
      a <- [1..n], b <- [1..n], c <- [n - a - b],
      a ^ 2 + b ^ 2 == c ^ 2]

--From Rosetta Code
--https://rosettacode.org/wiki/Factors_of_an_integer#Haskell
factors_o n = ds ++ [r | (d,0) <- [divMod n r], r <- r:[d | d>r]] ++ reverse (map (n `div`) ds)
        where
        r = floor (sqrt (fromIntegral n))
        ds = [i | i <- [1..r-1], mod n i == 0]

--Very fast for n = 1000 or n = 10000
--but takes 6.9 s to do n = 1010, say
--2 seconds for n = 1012.
--Takes a really long time to map to [1..1000]
problem9_from_factors :: Integer -> Maybe [Integer]
problem9_from_factors n = head' $ concat $ map triples_from_factor $ factors_o n
    where
        triples_from_factor k = map (scale k) (problem9general k)
        scale k = map (* (n `div` k))
        head' [] = Nothing
        head' (x:xs) = Just x


pairGCD :: Integer -> Integer -> Integer
pairGCD m 0 = m
pairGCD 0 m = m
pairGCD m n
    | n > m = pairGCD n m
    | otherwise = pairGCD n remainder
        where
            remainder = m - (m `div` n)*n 

--For input n (the desired sum), find r such that 2*r*(r+s) = n
--where r > s > 0 and (r,s) = 1
--Or return Nothing if it doesn't exist.
findSol :: Integer -> Maybe Integer
findSol n
    | odd n = Nothing
    | even n = head' $ filter isGood $ factors_o (n `div` 2)
        where
            head' [] = Nothing
            head' (x:xs) = Just x
            isGood r = (r > s) && (s > 0) && ((pairGCD r s) == 1)
                where
                    s = (n `div` (2*r)) - r

--From a solution r, get (r,s) where 2*r*(r+s) = n
processSol :: Integer -> (Maybe Integer) -> (Maybe (Integer, Integer))
processSol n Nothing = Nothing
processSol n (Just r) = Just (r, s)
    where
        s = (n `div` (2*r)) - r

findPair :: Integer -> Maybe (Integer,Integer)
findPair n = processSol n (findSol n)

findTriple :: Integer -> Maybe (Integer, Integer, Integer)
findTriple n = f (findPair n)
    where
        f Nothing = Nothing
        f (Just (r,s)) = Just (r^2 - s^2, 2*r*s, r^2 + s^2)

triples_from_factors :: Integer -> [Maybe (Integer, Integer, Integer)]
triples_from_factors n = map f $ factors_o n
    where
        f k = (scaleBy (n `div` k)) $ findTriple k
        scaleBy fact Nothing = Nothing
        scaleBy fact (Just (x,y,z)) = Just (fact*x,fact*y,fact*z)

--0.01 seconds for n = 1012
--Map this to [1..1000] in 0.3 seconds]
--Maps to [1..10000] in 4.2 seconds
problem9wicked :: Integer -> Maybe (Integer,Integer,Integer)
problem9wicked = f . triples_from_factors
    where
        f [] = Nothing
        f [Nothing] = Nothing
        f (Nothing:l) = f l
        f ((Just triple):l) = (Just triple)
