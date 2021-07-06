module Chap4.Core

import Data.Vect
import Data.Fin

data Shape = Triangle Double Double
    | Rectangle Double Double
    | Circle Double
%name Shape shape, shape', shape''


data Picture = Primitive Shape
    | Combine Picture Picture
    | Rotate Double Picture
    | Translate Double Double Picture
%name Picture pic, pic', pic''

public export
data BSTree : Type -> Type where
    Empty : Ord a => BSTree a
    Node : Ord a => BSTree a -> a -> BSTree a -> BSTree a

public export
data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr

%name Expr e, e', e''


insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)

export
listToTree : Ord a => List a -> BSTree a
listToTree = foldr insert Empty

export
treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

export
evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Sub e1 e2) = evaluate e1 - evaluate e2
evaluate (Mul e1 e2) = evaluate e1 * evaluate e2

export
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a 
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) (Just y) = Just $ max x y

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

export
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) = case shape of 
    Triangle _ _ => Just (area shape)
    _ => Nothing
biggestTriangle (Combine pic pic') = maxMaybe (biggestTriangle pic) (biggestTriangle pic')
biggestTriangle (Rotate x pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic

export
testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3)) (Primitive (Triangle 2 4))

export
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3)) (Primitive (Circle 4))

data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
    Unicycle : Vehicle Pedal
    Bicycle : Vehicle Pedal
    Motorcycle : (fuel : Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    ElectricCar : (battery : Nat) -> Vehicle Electricity
    Tram : (batter : Nat) -> Vehicle Electricity

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ElectricCar battery) = 4
wheels (Tram battery) = 4


refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

recharge : Vehicle Electricity -> Vehicle Electricity
recharge (ElectricCar battery) = ElectricCar 100
recharge (Tram battery) = Tram 200

export
vecTake : (k : Fin (S n)) -> Vect n a  -> Vect (finToNat k) a
vecTake FZ _ = []
vecTake (FS k) (x :: xs) = x :: (vecTake k xs) 

export
sumEntries : Num a => {n : Nat} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos v v'= case integerToFin pos n of
    Nothing => Nothing
    Just idx => Just (index idx v + index idx v')

    