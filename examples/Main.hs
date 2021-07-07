module Main where

import TestLib

main :: IO ()
main = do
    let y = canInline 10
        z = cannotInline 10
    print y
    print z
