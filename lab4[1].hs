import Data.List
import Numeric.Natural

produsRec :: [Integer] -> Integer
produsRec (x:xs) 
   |null xs = x 
   |otherwise = x * produsRec xs
  
produsFold :: [Integer] -> Integer
produsFold xs = foldr ( \x u n -> u ( n * x )) id xs 1

andRec :: [Bool] -> Bool
andRec (x:xs)
  |null xs = x
  |x == False = False 
  |otherwise = andRec xs

andFold :: [Bool] -> Bool
andFold xs = foldr (&&) True xs

concatRec :: [[a]] -> [a]
concatRec (x:xs)
  |null xs = x 
  |otherwise = x ++ concatRec xs
-- sau direct ultima linie + [] = []

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

rmChar :: Char -> String -> String

rmChar _ [] = []
rmChar a (x:xs)
  |null xs && (x==a) = []
  |null xs && (x /= a) = x:[]
  |x == a = rmChar a xs
  |otherwise = x : rmChar a xs

--rmChar _ [] = []
--rmChar c (h:t) = if c == h then rmChar c t else [h] ++ (rmChar c t) 
rmCharsRec :: String -> String -> String
rmCharsRec [] l = l
rmCharsRec (x:xs) h = rmChar x (rmCharsRec xs h)  


rmCharsFold :: String -> String -> String
rmCharsFold l h = foldr rmChar h l

logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
  where
    f 0 = start
    f n = rate * f (n - 1) * (1 - f (n - 1))


logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079
ex1 :: Natural
ex1 = 20


ex20 :: Fractional a => [a]
ex20 = [1, logistic0 ex1, 3]

ex21 :: Fractional a => a
ex21 = head ex20

ex22 :: Fractional a => a
ex22 = ex20 !! 2

ex23 :: Fractional a => [a]
ex23 = drop 2 ex20

ex24 :: Fractional a => [a]
ex24 = tail ex20


ex31 :: Natural -> Bool
ex31 x = x < 7 || logistic0 (ex1 + x) > 2

ex32 :: Natural -> Bool
ex32 x = logistic0 (ex1 + x) > 2 || x < 7
ex33 :: Bool
ex33 = ex31 5

ex34 :: Bool
ex34 = ex31 7

ex35 :: Bool
ex35 = ex32 5

ex36 :: Bool
ex36 = ex32 7


-- foldr f z (x:xs) = f x (foldr f z xs)