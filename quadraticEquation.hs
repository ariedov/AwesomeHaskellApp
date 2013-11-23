solveQuadratic :: Float -> Float -> Float -> (Float, Float)
solveQuadratic a b c = (x1, x2) 
	where   d  = findD a b c
		x1 = ((-b) + (sqrt d)) / (2 * a)
		x2 = ((-b) - (sqrt d)) / (2 * a)

findD :: Float -> Float -> Float -> Float
findD a b c = (b^2) - (4 * a * c)
  
