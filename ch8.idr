import Data.Vect

checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)
checkEqNat Z     Z     = Just Refl
checkEqNat Z     (S k) = Nothing
checkEqNat (S k) Z     = Nothing
checkEqNat (S k) (S j) = checkEqNat k j >>= Just . cong

myExactLength : (len : Nat) -> Vect m a -> Maybe $ Vect len a
myExactLength {m} len xs = checkEqNat m len >>= Just . \Refl => xs

-- 8.1.7 exercises
same_cons : {xs, ys : List a} -> xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : {xs, ys : List a} -> x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  CheckThreeEq : (x: a) -> ThreeEq x x x

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z (CheckThreeEq z) = CheckThreeEq (S z)
