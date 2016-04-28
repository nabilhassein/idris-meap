import Data.Vect

-- 3.2.4
myLength : List a -> Nat
myLength [] = Z
myLength (_ :: xs) = S (myLength xs)

myReverse : List a -> List a
myReverse = foldl (\acc, x => x :: acc) []

myMap : (a -> b) -> List a -> List b
myMap f = foldr (\x, acc => f x :: acc) []

myMapV : (a -> b) -> Vect n a -> Vect n b
myMapV _ []        = []
myMapV f (x :: xs) = f x :: myMapV f xs
