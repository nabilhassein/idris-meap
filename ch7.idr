-- 7.1.6
data Shape = Triangle Double Double | Rectangle Double Double | Circle Double

area : Shape -> Double
area (Triangle base height)    = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius)           = pi * radius * radius

Eq Shape where
  (Triangle b1 h1)  == (Triangle b2 h2)  = b1 == b2 && h1 == h2
  (Rectangle l1 h1) == (Rectangle l2 h2) = l1 == l2 && h1 == h2
  (Circle r1)       == (Circle r2)       = r1 == r2
  _                 == _                 = False

Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)


-- 7.2
data Expr num = Val num
              | Add (Expr num) (Expr num)              
              | Sub (Expr num) (Expr num)              
              | Mul (Expr num) (Expr num)              
              | Div (Expr num) (Expr num)              
              | Abs (Expr num)

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x)   = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x)   = abs (eval x)   

Num t => Num (Expr t) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg t => Neg (Expr t) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs


-- 7.2.4 exercises
(Neg t, Integral t, Eq t) => Eq (Expr t) where
  e1 == e2 = eval e1 == eval e2

(Neg t, Integral t) => Cast (Expr t) t where
  cast = eval


-- 7.3.4 exercises
Functor Expr where
  map f (Val x)   = Val (f x)
  map f (Add x y) = Add (map f x) (map f y)
  map f (Sub x y) = Sub (map f x) (map f y)
  map f (Mul x y) = Mul (map f x) (map f y)
  map f (Div x y) = Div (map f x) (map f y)
  map f (Abs x)   = Abs (map f x)


data Vect : Nat -> Type -> Type where
  Nil  : Vect 0 a
  (::) : a -> Vect k a -> Vect (S k) a

Eq a => Eq (Vect n a) where
  []        == []        = True
  (x :: xs) == (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr _ acc []        = acc
  foldr f acc (x :: xs) = f x $ foldr f acc xs

