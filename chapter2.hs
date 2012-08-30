module Main where
	
	squares l = map square l
		where square x = x * x
		
	myRange start step  = start:(myRange (start + step) step)
	
	lazyFib x y = x:(lazyFib y (x+y))
	
	fib = lazyFib 1 1
	
	fibNth x = head (drop (x - 1) (take (x) fib))