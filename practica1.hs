--1--
--a--
borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs 
--b--


--c--
serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie (init xs) ++ [xs]
--d--
paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d 
                | a == b && c == d = True
                | a == c && b == d = True
                | a == d && b == c = True
                | otherwise = False
--e--
isosceles :: Int -> Int -> Int -> Bool
isosceles l1 l2 l3 
                | l1 == l2 && l1 /= l3 = True
                | l1 == l3 && l1 /= l2 = True
                | l2 == l3 && l1 /= l2 = True 
                | otherwise = False
--f--
ror 0 xs = xs  
ror _ [] = []
ror n (x:xs) = ror (n-1) (xs ++ [x])

--g--
upto :: Int -> Int -> [Int]
upto n m 
        | n > m = []
        |  otherwise = n : upto (n + 1) m
