import Data.List
factori :: Int -> [Int]
factori n = [x| x <- [1..n] , n `mod` x == 0 ]

prim :: Int -> Bool
prim n = if length (factori n) > 2 
         then False
         else
          if length (factori n) > 1
          then True 
          else False

numerePrime :: Int -> [Int]
numerePrime n = [x| x <-[1..n] , (prim x) == True]

zipo :: [a] -> [b] -> [c] -> [(a,b,c)]
zipo (a:as) (b:bs) (c:cs) = (a,b,c) : zipo as bs cs
zipo _ _ _ = []
--myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
--myzip3 l1 l2 l3 = zipo l1 l2 l3 frt

firstEl :: [(Char,Int)] ->[Char]
firstEl [] = []
firstEl l = [x| (x,y) <- l]

suma :: [Int] -> Int
suma [] = 0
suma (x:l)
   | null l = x
   | otherwise = x + suma l

sumList :: [[Int]] -> [Int]
sumList l = [x | y <- l, let x = suma y]

prel2 :: [Int] -> [Int]
prel2 [] = []
prel2 (x:l) 
   |even x = (x `div` 2) : prel2 l 
   |otherwise = (x*2) : prel2 l

--firstEl l = map(\(x, _) -> x) l 
--sumList l = map sum l 
--aux x = if mod x 2 == 0 then div x 2 else x * 2 
--prel2 lista = map aux lista 
--prel2` lista = map (\x -> if mod x 2 == 0 then div x 2 else x * 2 ) lista 

ex3 :: [Int] -> [Int] 
ex3 l = 
 -- let pozImpar (_, pos) = odd pos in 
  let lfilter = filter (\(_, y) -> odd y)(zip l [1..]) in 
  map (\ (x, _) -> x^2 ) lfilter 

-- fct care primeste ca param o lista si un nr si aduna 
exextra l x = map (+x) l 
--exextra l x = map(\y -> y + x)l

--3.4

ex :: Char -> [String] -> [String]

ex c t = filter (elem c) t 

patrat :: [Int] -> [Int]

patrat l =map (\x -> x^2) (filter odd l)

patratpoz :: [Int] ->[Int]

patratpoz l  = map (\(x,_) -> (x^2)) (filter (\(_,y) -> (odd y)) (zip l [1..]))

numaiVocale :: [[Char]] -> [[Char]]
numaiVocale l = map (\prop -> filter ( `elem` "aeiouAEIOU") prop) l
