module Main ( main ) where

import TestLib

foo :: Int -> (Int,Int)
foo n = (canInline n, cannotInline n)

main :: IO ()
main = print (foo 10)
