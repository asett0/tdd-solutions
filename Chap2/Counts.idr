module Chap2.Counts

import Chap2.Core
import System.REPL

main : IO ()
main = repl "Enter a string: " $ shown . counts
