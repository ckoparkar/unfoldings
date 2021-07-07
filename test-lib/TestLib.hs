module TestLib where

cannotInline :: Int -> Int
cannotInline x = x + 1

{-# INLINE canInline #-}
canInline :: Int -> Int
canInline x = x + 1
