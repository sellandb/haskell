main :: IO ()
main = putStrLn "Hello, world!"

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, mod x 2 == 1]

length' xs = sum [ 1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, elem c ['A'..'Z']]

triangles = [(x, y, z) | z<-[1..10], x <- [1..z], y<-[1..x],  ((x^2) + (y^2)) == (z^2), x+y+z == 24]
