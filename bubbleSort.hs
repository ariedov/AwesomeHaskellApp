bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (x:y:xs)
	| x > y 	= y:x:bubbleSort xs
	| otherwise 	= x:bubbleSort (y:xs)

-- stoled from wiki
bsort :: Ord a => [a] -> [a]
bsort s = case _bsort s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where _bsort (x:x2:xs) | x > x2    = x2:(_bsort (x:xs))
                         | otherwise = x:(_bsort (x2:xs))
        _bsort s = s

