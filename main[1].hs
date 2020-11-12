import Data.List
import Data.Char
import Data.List
myInt = 55555555555555555555555555555555555555555555555555555555555

maxim :: Integer -> Integer ->Integer
maxim x y = if (x > y) then x else y

maxim3 x y z = maxim x (maxim y z)

par x = do if (x `mod` 2) == 0
           then print "par"
           else
               print "impar"

fact x = if x == 0
         then 1
         else 
             x*fact(x-1)


verif x y = if (x > 2*y)
            then print "yes"
            else print "no"
--fibonacci
fibonn :: Integer -> Integer
fibonn n
    | n < 2 =n
    | otherwise = fibonn (n -1) + fibonn(n-2)

semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l
  | null l = l
  | even h = h `div` 2 : t'
  | otherwise = t'
  where
  h = head l
  t = tail l
  t' = semiPareRecDestr t

semiPare :: [Int]  -> [Int]
semiPare l = [x `div` 2 | x <-l , even x]


inInterval :: Int -> Int -> [Int] -> [Int]
inInterval h t l= [ x | x <- l , x >= h , x <= t]
inIntervalRec :: Int -> Int -> [Int] -> [Int]


isin x h t = if (x <= t)
             then if (x >= h)
               then 1
               else 0
             else 0
inIntervalRec h t [] = []
inIntervalRec h t (x:xs) 
  | isin x h t ==1 = x : inIntervalRec h t xs
  | otherwise = inIntervalRec h t xs

inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp lo hi xs = [x| x <-xs, x>=lo , x <= hi]

pozitiveRec :: [Int] -> Int

pozitiveRec l  
  | null l = 0
  | 0 - x < 0 = 1 + pozitiveRec (tail l)
  | otherwise = 0 + pozitiveRec(tail l)
  where
  x = head l



pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x| x<-l, x > 0]


add :: Int -> [Int] ->[Int]
add x l
  | null l = [x]
  |otherwise = x:l

pozitiiImpareRecA :: [Int] -> Int -> [Int]
pozitiiImpareRecA [] a = []
pozitiiImpareRecA l a 
   |odd x = y : pozitiiImpareRecA (tail l) (a+1)
   |otherwise = pozitiiImpareRecA (tail l) (a+1)
   where
   x = head l
   y = a 

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = pozitiiImpareRecA l 0

findodd :: [Int] -> Int -> [Int]
findodd (x:l) a
    |null l = []
    |odd x = a : findodd l (a+1)
    |otherwise = findodd l (a+1)

--ma fol de recursiva si aia e
pozitiiImpareCompA :: [Int] -> Int -> [Int]
pozitiiImpareCompA l a = [x|  x <- pozitiiImpareRec l]

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = pozitiiImpareCompA l 0


isDigit' :: Char -> Bool
isDigit' c = (c >= '0') && (c <= '9')

digitToInt' :: Char -> Int
digitToInt' c 
   | isDigit' c = fromEnum c - fromEnum '0'


mult :: Char -> Int -> Int 
mult x a 
   |isDigit' x = a * (digitToInt' x) 
   |otherwise = a
multDigitsReca :: String -> Int -> Int
multDigitsReca (x:s) a 
    |null s = mult x a
    |isDigit' x = multDigitsReca s (a * (digitToInt' x) )
    |otherwise = multDigitsReca s a 


multDigitsRec :: String -> Int
multDigitsRec s = multDigitsReca s 1