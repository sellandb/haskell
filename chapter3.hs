module Main where
	data Suit = Spades | Hearts | Clubs | Diamonds deriving (Show)
	data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	type Card = (Rank, Suit)
	type Hand = [Card]
	
	value :: Rank -> Integer
	value Two = 2
	value Three = 3
	value Four = 4
	value Five = 5
	value Six = 6
	value Seven = 7
	value Eight = 8
	value Nine = 9
	value Ten = 10
	value Jack = 11
	value Queen = 12
	value King = 13
	value Ace = 14
	
	cardValue :: Card -> Integer
	cardValue (rank,suit) = value rank
	
	backwards :: [a] -> [a]
	backwards [] = []
	backwards (h:t) = backwards t ++ [h]
	
	data Triplet a = Trio a a a deriving (Show)
	
	data Tree a = Children [Tree a] | Leaf a deriving (Show)
	
	
	treeDepth (Leaf _) = 1
	treeDepth (Children c) = 1 + maximum (map treeDepth c)
	
	data Position t = Position t deriving (Show)
	
	stagger (Position d) = Position ( d + 2 )
	crawl (Position d) = Position ( d + 1 )
	
	rtn x = x
	x >>== f = f x
	
	treasureMap pos = pos >>==
						stagger >>==
						stagger >>==
						crawl >>==
						rtn
						
	tryIo = do putStr "Enter your name: ";
				line <- getLine;
				let { backwards = reverse line};
				return ("Hello, Your name backwards is " ++ backwards)
				
	crack = do x <- ['a' .. 'f'] ; y <- ['a' .. 'f'] ; z <- ['a' .. 'f'];
				let { password = [x,y,z] };
				if attempt password
					then return (password, True)
					else return (password, False)
					
	attempt pw = if pw == "fad" then True else False