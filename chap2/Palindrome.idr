module Palindrome

import Chap2
import System.REPL

main : IO ()
main = repl "Enter a string: " $ shown . palindrome
