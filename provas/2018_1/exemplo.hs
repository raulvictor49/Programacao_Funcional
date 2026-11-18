divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

primo :: Int -> Bool
primo n = divisores n == [1,n]

primos :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]