import Data.Char
import Data.List 
import Data.Char (toUpper)
--1
rotateaux :: Int ->[Char]->[Char]
rotateaux n (x:l)
  | null l = x : []
  | n < 0 = error "n<0"
  | n > length l = error "n este prea mare"
  | n == 0  = []
  | otherwise = x:rotateaux (n-1) l


drop' :: Int -> [a] -> [a]
drop' _ [] = []            -- dropping any number of items from [] is still empty
drop' 0 lst = lst          -- dropping nothing returns the list unchanged
drop' n (_:xs) = drop' (n-1) xs  -- otherwise remove head and recurse on the tail

rotate :: Int ->[Char] -> [Char] 
rotate n l = (drop' n l) ++ (rotateaux n l)
-- take ia primele 
-- drop sare peste
-- rescrie : else (drop n l) ++ (take n l)
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l
--3

laste :: [a] -> a
laste = foldr1 (\_ a -> a)

makeKeyaux :: Int ->[Char] ->[Char]

p = ['A'..'Z']
makeKeyaux n l
  | null l = []
  | otherwise  = laste(rotate n (l++p)): makeKeyaux n (tail l)

zipo :: [a] -> [b] -> [(a,b)]
zipo (a:as) (b:bs) = (a,b) : zip as bs
zipo _      _      = []

makeKey :: Int ->[(Char,Char)]

makeKey n = zipo p (makeKeyaux (n+1) p)
--makeKey n= zip p (rotate n p)
--4

lookUp :: Char -> [(Char,Char)] -> Char 

lookUp a l 
  | null l = a 
  | fst x == a = snd x 
  | otherwise = lookUp a (tail l)
  where
  x = head l 


--lookUp c cifru  = let filter(\(x,y) -> x == c ) cifru in 
--if l ==[] then c else snd (head l)
--5

encipher :: Int -> Char -> Char 
encipher n c = lookUp c (makeKey n)

--6
normalize :: String -> String

fct :: Char -> Bool 
fct a = if (a >= 'a' && a <= 'z') 
        then True
        else
          if (a >='A' && a <= 'Z')  || ( a >= '0' && a <= '9')
          then True
          else False

normalize p = map toUpper (filter fct p)
--isDigit, isLower 
--7 

encipherStr :: Int -> String -> String 
encipherStr n l = map (encipher n) (normalize l)

--8 
reverseKey :: [(Char,Char)] -> [(Char,Char)]
reverseKey l = zipo (map snd l) (map fst l)

--9 

lookUp' :: Char -> [(Char,Char)] -> Char 

lookUp' a l 
  | null l = a 
  | snd x == a = fst x 
  | otherwise = lookUp' a (tail l)
  where
  x = head l 

decipher :: Int -> Char -> Char 
decipher n a = lookUp' a (makeKey n)
--decipher = lookUp a reverseKey  (makeKey n )
--9

normalize' :: String -> String

fct' :: Char -> Bool 
fct' a = if (a >= 'a' && a <= 'z') 
        then False
        else
          if (a >='A' && a <= 'Z')  || ( a >= '0' && a <= '9') || ( a =='-')
          then True
          else False

normalize' p = map toUpper (filter fct' p)

decipherStr :: Int -> String -> String 
decipherStr n l = map (decipher n) (normalize' l)
 

data Rezultat = Cv|Altc deriving(Show) 
--vrea sa fie cu litera mare
--un cosntructor e felul in care se constr un anumit tip 
-- la liste constr : []

--f [a,b,c] -> a:b:c:[]

-- ,: e si constr si functie 
-- foldre (:) [1,2] [3,4,5] = [3,4,5,1,2]
--operator poate fie scris cum vrei
--a % b == mod a b
--a // b = div a b 
--a +-+ = a^2 + b^2

--cica exista 
-- <->
-- <=> 
-- ==>
-- >>>
-- <<<
-- cosntructorii sunt cerc patrat Dreptunghi
data Figura = Cerc Float | Patrat Float |Dreptunghi Float Float deriving(Show) 
--deriving afiseaza val deriva functionalitatea de afisare...implementeaza interfata care afiseaza pe ecran

aria (Cerc l) = raza ^ 2 * pi 
aria (Patrat latura) = latura * 2
aria (Dreptunghi lungime latime) = lungime * latime 

--data Lista = Goala | Append a (Lista a) deriving(Show) 

--sum' Goala = 0 
--sum' (Append x tail) = x* (sum' tail) 

--data Either a b = Sau a | Altfel b deriving(Show)
-- ::: constructor
--data List a = Empty | a ::: (List a ) deriving (Show)

