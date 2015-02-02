module Main where

import Test.DocTest

main :: IO ()
main = doctest [
                   "src/Tutorial.hs"
                 , "src/FunnyMath.hs"
                 , "src/Main.hs"
               ]
