solveQuadratic :: Float -> Float -> Float -> (Float, Float)
solveQuadratic a b c = solve a b $ findD a b c

findD :: Float -> Float -> Float -> Float
findD a b c = (b^2) - (4 * a * c)

solve :: Float -> Float -> Float -> (Float, Float)
solve a b d
	| d >= 0	= (x1, x2)
	| d < 0		= error "There is no solution"
	where 	x1 = ((-b) + (sqrt d)) / (2 * a)
		x2 = ((-b) - (sqrt d)) / (2 * a)

 	
