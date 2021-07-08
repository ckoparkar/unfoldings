module TestLib where

-- {-# NOINLINE cannotInline #-}
cannotInline :: Int -> Int
cannotInline n =
    if n == 0
    then 0
    else if n == 1
    then 1
    else
        let x = cannotInline (n - 1)
            y = cannotInline (n - 2)
        in x + y

{-# INLINE canInline #-}
canInline :: Int -> Int
canInline n = cannotInline n
