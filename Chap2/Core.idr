module Chap2.Core

import Data.Strings
import Data.List

export
shown : Show ty => ty -> String
shown x = show x ++ "\n"

export
palindrome : String -> Bool
palindrome s = let ls = toLower s in
    reverse ls == ls

palindrome_length : Nat -> String -> Bool
palindrome_length n s = let ls = toLower s in
    if (length ls > n) then reverse ls == ls else False

export
counts : String -> (Nat, Nat)
counts s = (length $ words s, length s)

top_ten : Ord a => List a -> List a
top_ten = take 10 . reverse . sort

over_length : Nat -> List String -> Nat
over_length n = length . filter (>n) . map length
