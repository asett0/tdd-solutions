module Chap2.Palindrome

import Chap2.Core
import System.REPL

main : IO ()
main = repl "Enter a string: " $ shown . palindrome
