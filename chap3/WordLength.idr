import Data.Vect

allLengths : Vect n String -> Vect n Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words


insert : Ord elem => elem -> Vect len elem -> Vect (S len) elem
insert x [] = [x]
insert x (y :: ys) = case x < y of
                          False => y :: insert x ys
                          True => x :: y :: ys

inSort : Ord elem => Vect n elem -> Vect n elem
inSort [] = []
inSort (x :: xs) = insert x $ inSort xs

my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = S $ my_length xs

my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x] 

my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

my_vect_map : (a -> b) -> Vect n a -> Vect n b
my_vect_map f [] = []
my_vect_map f (x :: xs) = f x :: my_vect_map f xs

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
