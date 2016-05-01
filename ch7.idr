import Data.Vect

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
