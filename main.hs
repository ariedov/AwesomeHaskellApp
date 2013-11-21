doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
			then x
			else x * 2
doubleSmallNumber' x = doubleSmallNumber x + 1
conanO'brian = "Hello, this is Conan O'Brian"
tenEvenNumbers = [x*2 | x <- [1..10]]
threeMoreFromSeven = [x | x <- [50..100], x `mod` 7 == 3]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [ 1 | _ <- xs]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  

head' (x:_) = x

length'' [] = 0
length'' (_:xs) = 1 + length' xs

maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
	| x > maxTail =x
	| otherwise = maxTail
	where maxTail = maximum' xs

replicate' n x 
	| n <= 0 = []
	| otherwise = x:replicate' (n-1) x

take' n _
	| n <= 0 = []
take' _ [] 	 = []	
take' n (x:xs) = x : take' (n-1) xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]
repeat' x = x:repeat' x

zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' a [] = False
elem' a (x:xs)
	| a == x = True
	| otherwise = a `elem'` xs
