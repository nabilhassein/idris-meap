import Data.Fin
import Data.Vect

-- 4.1.5

-- a binary search tree
data BSTree : (a : Type) -> Type where
  Empty : Ord a => BSTree a
  Node : Ord a => (left : BSTree a) -> (val: a) -> (right : BSTree a) -> BSTree a

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
  LT => Node (insert x left) val right
  EQ => orig
  GT => Node left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree = foldl (\tree, x => insert x tree) Empty

treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right


data Expr = Val Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

evaluate : Expr -> Integer
evaluate (Val x)   = x
evaluate (Add x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mul x y) = evaluate x * evaluate y


maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing   = Nothing
maxMaybe (Just x) Nothing  = Just x
maxMaybe Nothing (Just y)  = Just y
maxMaybe (Just x) (Just y) = Just (max x y)


data Shape = Triangle Double Double
 | Rectangle Double Double
 | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
 | Combine Picture Picture
 | Rotate Double Picture
 | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle _ _)) = Just (area t)
biggestTriangle (Primitive _)                = Nothing
biggestTriangle (Combine p1 p2)              = maxMaybe (biggestTriangle p1) (biggestTriangle p2)
biggestTriangle (Rotate _ p)                 = biggestTriangle p
biggestTriangle (Translate _ _ p)            = biggestTriangle p


-- selected from 4.2.4
take : (k : Fin (S n)) -> Vect n a -> Vect (finToNat k) a
take FZ     _         = []
take (FS p) (x :: xs) = x :: take p xs

sumEntries : Num a => {n : Nat} -> Integer -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} k xs ys = do
  i <- integerToFin k n
  Just $ index i xs + index i ys
