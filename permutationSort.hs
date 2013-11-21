psort :: (Ord a) => [a] -> [a]
psort [] = []
psort [x] = [x]
psort (xs) = 
	min:psort restOfList
	where  	min = minInList xs
		restOfList = [a | a <- xs, a /= min]	
	

minInList :: (Ord a) => [a] -> a
minInList [] = error "No min in empty list"
minInList [x] = x
minInList (x:y:xs)
	| x < y 	= minInList (x:xs)
	| otherwise 	= minInList (y:xs) 
