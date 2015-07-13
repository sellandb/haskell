main :: IO ()
main = putStrLn "Hello, world!"

lucky :: (Integral a) => a -> String
lucky 7 = "Lucky Number Seven!!"
lucky x = "Better Luck Next Time."

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x - 1)
