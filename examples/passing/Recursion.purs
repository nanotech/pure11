module Recursion where

  fib = \n -> case n of
    0 -> 1
    1 -> 1
    n -> fib (n - 1) + fib (n - 2)
    
module Main where

main = Trace.trace "Done"
