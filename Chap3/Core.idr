module Chap3.Core

import Data.Vect
import Data.Zippable

export
my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S $ my_length xs

export
my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x] 

export
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

export 
my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

createEmpties : {n: _} -> Vect n (Vect 0 elem)
createEmpties = replicate _ []

export
transposeMat : {n : _} -> Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
    zipWith (::) x xsTrans

export
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs ys = zipWith (zipWith (+)) xs ys


multHelper : Num a => (x : Vect m a) -> (ysTrans : Vect p (Vect m a)) -> Vect p a
multHelper _ [] = []
multHelper x (y :: ys) = (sum $ zipWith (*) x y) :: multHelper x ys 

export
multMatrix : Num a => {p : _} -> Vect (n) (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] _ = []
multMatrix (x :: xs) ys = let ysTrans = transposeMat ys in
    multHelper x ysTrans :: multMatrix xs ys