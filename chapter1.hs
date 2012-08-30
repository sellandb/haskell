module Main where
	double x = x + x
	
	strongdouble :: Integer -> Integer
	strongdouble x = x + x
	
	factorial :: Integer -> Integer
	factorial 0 = 1
	factorial x = factorial(x - 1) * x
	
	guardfactorial :: Integer -> Integer
	guardfactorial x
		| x > 1 = x * guardfactorial (x - 1)
		| otherwise = 1
		
	fib :: Integer -> Integer
	fib 1 = 1
	fib 2 = 1
	fib x = fib(x - 1) + fib(x - 2)
	
	fibtuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
	fibtuple (x, y, 0) = (x, y, 0)
	fibtuple (x, y, index) = fibtuple (y, x + y, index - 1)
	
	fibresult :: (Integer, Integer, Integer) -> Integer
	fibresult (x, y, z) = x
	
	fib2 :: Integer -> Integer
	fib2 x = fibresult(fibtuple(0,1,x))
	
	fibNextPair :: (Integer, Integer) -> (Integer, Integer)
	fibNextPair (x,y) = (y, y+x)
	
	fibNthPair :: Integer -> (Integer, Integer)
	fibNthPair 1 = (1,1)
	fibNthPair n = fibNextPair(fibNthPair (n - 1))
	
	fib3 :: Integer -> Integer
	fib3 = fst . fibNthPair
	
	size :: [a] -> Integer
	size [] = 0
	size (h:t) = 1 + size t
	
	prod :: [Integer] -> Integer
	prod [] = 1
	prod (h:t) = h * (prod t)
	
	alleven :: [Integer] -> [Integer]
	alleven [] = []
	alleven (h:t) = if even h then h : alleven t else alleven t
	
	alleven2 :: [Integer] -> [Integer]
	alleven2 x = [y | y <- x, even y]
	
	reverselistinner :: ([a], [a])  -> [a]
	reverselistinner ([], x) = x
	reverselistinner (h:t, x) = reverselistinner (t, h : x)
	
	reverselist :: [a] -> [a]
	reverselist x = reverselistinner(x, [])
	
	colors = ["red", "green", "blue"]
	colorslist = take 1 [(ten, mis, ala, geo, florida) | ten <- colors, mis <- colors, ala <- colors, geo <- colors, florida <- colors, mis /= ten, mis /= ala, ala /= ten, ala /= mis, ala /= geo, ala /= florida, geo /= florida, geo /= ten]
	