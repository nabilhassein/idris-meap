import Data.Vect

%default total

-- 3.2.4
myLength : List a -> Nat
myLength = foldl (\acc, _ => acc + 1) 0

myReverse : List a -> List a
myReverse = foldl (\acc, x => x :: acc) []

myMap : (a -> b) -> List a -> List b
myMap f = foldr (\x, acc => f x :: acc) []

myMapV : (a -> b) -> Vect n a -> Vect n b
myMapV _ []        = []
myMapV f (x :: xs) = f x :: myMapV f xs

-- 3.3.3
Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)

transpose_mat : Matrix n m a -> Matrix m n a
transpose_mat []            = replicate _ []
transpose_mat (row :: rows) = zipWith (::) row (transpose_mat rows)

add_matrix : Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
add_matrix []        []        = []
add_matrix (x :: xs) (y :: ys) = zipWith (+) x y :: add_matrix xs ys

mul_matrix : Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
mul_matrix []            _  = []
mul_matrix (row :: rows) m2 = map (sum . zipWith (*) row) (transpose_mat m2) :: mul_matrix rows m2

test_m1 : Matrix 3 2 Int
test_m1 = [
  [1, 2],
  [3, 4],
  [5, 6]
]

test_m2 : Matrix 2 4 Int
test_m2 = [
  [7, 8, 9, 10],
  [11, 12, 13, 14]
]

actual : Matrix 3 4 Int
actual = mul_matrix test_m1 test_m2

expected : Matrix 3 4 Int
expected = [
  [29, 32, 35, 38],
  [65, 72, 79, 86],
  [101, 112, 123, 134]
]

mul_matrix_test : Main.actual = Main.expected
mul_matrix_test = Refl
