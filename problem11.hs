import Data.List

--Prionic's solution

maxQuadInGrid grid = 
    let
	horizontals = id
	verticals = transpose
	diagonalsQuarter = transpose.zipWith drop [0..]
	localmaxima = map (maximum. map (product. take 4). tails)
	join = concatMap (flip ($) grid)
    in
	maximum.
	localmaxima.
	join $
	    [horizontals
	    ,verticals
	    ,diagonalsQuarter
	    ,diagonalsQuarter. reverse
	    ,diagonalsQuarter. map reverse
	    ,diagonalsQuarter. map reverse. reverse] 

main = do
    contents <- readFile "problem11.txt"
    let array = [[read n | n <- words l] | l <- lines contents] :: [[Int]]
    print $ maxQuadInGrid array
