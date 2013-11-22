fizzbuzz = map divisionCheck [1..100]

divisionCheck :: Integer -> String
divisionCheck n
	| div5 (div3 n) == 0	= "FizzBuzz"
	| div3 n  == 0 	 	= "Fizz"
	| div5 n  == 0 		= "Buzz"
	| otherwise		= show n 
	where 	div3 n = n `rem` 3
		div5 n = n `rem` 5
