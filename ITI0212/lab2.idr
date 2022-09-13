module Main

main : IO ()
main = putStrLn "Hello world"


-- only one
q1a: Bool -> ()
q1a a = ()

-- number of functions is output^input
q1b : List (Bool -> Bool)
q1b = [one, two ,three ,four] where 
    one : Bool -> Bool
    one a = a 
    two : Bool -> Bool
    two a = not a 
    three : Bool -> Bool
    three _ = True
    four : Bool -> Bool
    four _ = False

-- only one
q1c : Nat -> Unit
q1c _ = ()

-- infinite number of functions, one of them is 
q1d : Unit -> Nat
q1d () = 0

-- no functions can return the void - it cannot be constructed
q1e : Void -> Void 
q1e _ = ?hole

q1f : Nat -> Void
q1f _ = ?hole1
-- infinite number of functions can be defined however they cannot be called
q1g : Void -> Nat
q1g _ = 0


---
data Shape : Type where
    Circle : Nat -> Shape
    Rectangle : Nat -> Nat -> Shape
    IsoTriangle : Nat -> Nat -> Shape -- base - other two
    Polygon : Nat -> Nat -> Shape  --sidelen sides
    IsoTriangle' : Nat -> Nat -> Shape -- base - height


--formulas can be wrong i did not test them
area : Shape -> Double
area (Circle radius) = pi * (cast (radius * radius))
area (Rectangle k j) = (cast(k * j)) 
area (IsoTriangle b s) = (1 / 2) * b' * (sqrt (s' * s' - (b' * b' / 4))) where 
    b' : Double
    b' = cast b
    s' : Double
    s' = cast s

area (Polygon s n) = p * a / 2 where 
 p : Double
 p = cast(s*n)
 a : Double
 a = cast(s) / (2 * tan( pi / cast(n) ))   
area (IsoTriangle' b h) = cast (b * h)  / 2

regular : Shape -> Bool
regular (Circle k) = False
regular (Rectangle k j) = k == j
regular (IsoTriangle k j) = k == j
regular (Polygon k j) = True
regular (IsoTriangle' b h) = b' * b' == (cast(h * h) + (b'*b' / 4)) where 
  b' : Double
  b' = cast(b)

 
monus : Nat -> Nat -> Nat 
monus Z _ = Z
monus x Z = x
monus (S k) (S l) = monus k l

even : Nat -> Bool
even Z = True
even (S k) = not (even k)

odd : Nat -> Bool
odd Z = False
odd (S k) =   even k 