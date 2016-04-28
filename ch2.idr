module Main

palindrome : String -> Bool
palindrome s = s == reverse s

longPalindrome : Nat -> String -> Bool
longPalindrome n s = (length s >= n) && (s == reverse s)

counts : String -> (Nat, Nat)
counts s = (length (words s), length s)

top_ten : Ord a => List a -> List a
top_ten = reverse . Prelude.List.take 10 . reverse . sort

over_length : Nat -> List String -> Nat
over_length n = length . filter (\x => length x > n)
