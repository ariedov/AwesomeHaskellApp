findPI x = sum [4 / if r `elem` [3,7..x*4]
		then r * (-1) 
		else r 
	| r <- [1,3..x], x > 3]
