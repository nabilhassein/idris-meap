import Data.Vect

checkEqNat : (n : Nat) -> (m : Nat) -> Maybe (n = m)
checkEqNat Z     Z     = Just Refl
checkEqNat Z     (S k) = Nothing
checkEqNat (S k) Z     = Nothing
checkEqNat (S k) (S j) = checkEqNat k j >>= Just . cong

myExactLength : (len : Nat) -> Vect m a -> Maybe $ Vect len a
myExactLength {m} len xs = checkEqNat m len >>= Just . \Refl => xs
