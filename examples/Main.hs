{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main ( main ) where

import TestLib
import qualified Data.HashMap.Lazy as M

foo :: Int -> (Int,Int)
foo n = (canInline n, cannotInline n)

yoo :: M.HashMap a b -> Int
yoo = M.size

main :: IO ()
main = print (foo 10)
