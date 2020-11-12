import Data.List (nub)
import Data.Maybe (fromJust)


data Fruct
    = Mar String Bool
    | Portocala String Int
      deriving(Show)


ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False
ePortocalaDeSicilia (Portocala "Tarocco" _) = True
ePortocalaDeSicilia (Portocala "Moro" _) = True
ePortocalaDeSicilia (Portocala "Sanguinello" _) = True
ePortocalaDeSicilia (Portocala _ _) =False
test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

trd' :: Fruct -> Int

trd' (Portocala _ z) = z 

trd :: Fruct -> Bool
trd (Mar _ z) = z == True
trd (Portocala _ _) = False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l = foldr(\ x acc n -> acc (n+ (trd' x))) id (filter ePortocalaDeSicilia l) 0 

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

eMar :: Fruct -> Bool
eMar (Portocala _ _) = False
eMar (Mar _ _) = True

nrMereViermi :: [Fruct] -> Int
nrMereViermi l = length (filter trd l)  
test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

rasa :: Animal -> String
rasa (Pisica _) = "Nothing"
rasa (Caine _ z) = z

type Nume = String
data Prop
    = Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    deriving (Eq, Read)
infixr 2 :|:
infixr 3 :&:

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

p2 :: Prop
p2 = (Var "P" :|: Var"Q") :&: (Not (Var "P") :&: Not (Var "Q")) 

p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not (Var "R")))

--ex2

instance Show Prop where
  show (Var s) = show s
  show (Not p) = "(" ++ show p ++ ")"
  show (p1 :|: p2) = "(" ++ show p1 ++ "|" ++ show p2 ++ ")"
  show (p1 :&: p2) = "(" ++ show p1 ++ "&" ++ show p2 ++ ")"

test_ShowProp :: Bool
test_ShowProp = show (Not (Var "P") :&: Var "Q") == "((~P)&Q)"

type Env = [(Nume, Bool)]

impureLookup :: Eq a => a -> [(a,b)] -> b
impureLookup a = fromJust . lookup a

eval :: Prop -> Env -> Bool
eval (Var x) env = impureLookup x env 
eval (Not x) env = not (eval x env)
eval (p1 :|: p2) env = (eval p1 env) || (eval p2 env)
eval (p1 :&: p2) env = (eval p1 env) && (eval p2 env)

test_eval = eval  (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True

--ex 4

variabile :: Prop -> [Nume]
variabile (Var x) = x:[]
variabile (Not x) = variabile x
variabile (p1 :|: p2) = nub (variabile p1 ++ variabile p2)
variabile (p1 :&: p2) = nub (variabile p1 ++ variabile p2)

test_variabile = variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

envs :: [Nume] -> [Env]
envs [] = []

envs (x:l) = ([(x,False)] : (envs l)) ++ ([(x,True)] : (envs l))

test_envs =
      envs ["P", "Q"]
      ==
      [ [ ("P",False)
        , ("Q",False)
        ]
      , [ ("P",False)
        , ("Q",True)
        ]
      , [ ("P",True)
        , ("Q",False)
        ]
      , [ ("P",True)
        , ("Q",True)
        ]
      ]

 


      