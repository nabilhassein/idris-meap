import Data.Vect

data EqNat : Nat -> Nat -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : EqNat k j -> EqNat (S k) (S j)
sameS (Same j) = Same (S j)

checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (EqNat n m)
checkEqNat Z     Z     = Just $ Same 0
checkEqNat Z     (S k) = Nothing
checkEqNat (S k) Z     = Nothing
checkEqNat (S k) (S j) = checkEqNat k j >>= Just . sameS

myExactLength : (len : Nat) -> Vect m a -> Maybe (Vect len a)
myExactLength {m} len xs = checkEqNat m len >>= Just . (\(Same _) => xs)
