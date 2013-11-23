solveQuadratic :: (Num a, Fractional a, Floating a, Ord a) => a -> a -> a -> (a, a)
solveQuadratic a b c = solve a b $ findD a b c

findD :: (Num a, Fractional a, Floating a, Ord a) => a -> a -> a -> a
findD a b c = (b^2) - (4 * a * c)

solve :: (Ord a, Num a, Fractional a, Floating a) => a -> a -> a -> (a, a)
solve a b d
	| d >= 0	= (x1, x2)
	| d < 0		= error "There is no solution"
	where 	x1 = ((-b) + (sqrt d)) / (2 * a)
		x2 = ((-b) - (sqrt d)) / (2 * a)

 	
