import Data.Char
import Test.QuickCheck 
type Cifra = Int
type Numar = [Cifra]

lungimePlus :: Numar -> Int -> Numar
lungimePlus numar 0 = numar
lungimePlus numar z = lungimePlus (0:numar (z -1)


normalizeazaLungilme ::(Numar, Numar) ->(Numar, Numar)

normalizeazaLungilme (l1,l2) = 
  let x = length l1 in 
  let y = length l2 in 
  let z = max(x, y) in 
  (lungimePlus l1 (z-x),lungimePlus l2 (z-y) )