fibs = 1:2:zipWith(+) fibs (tail fibs)
problem2 = sum . takeWhile (< 4000000) . filter even $ fibs
