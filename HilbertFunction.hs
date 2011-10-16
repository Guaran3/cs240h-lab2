-- functions to convert from xy coordinates to hilbert, and
-- from hilber to x,y.


module HilbertFunction where

import Data.Word
import Data.Bits

xy2d :: Word32 -> Word32 -> Word32 -> Word32 
xy2d size x y = xy2dr size (x, y) 0 where
    xy2dr 0 (x, y) total = total
    xy2dr s (x, y) total = xy2dr (s `div` 2) rot (total + inc) where
        rx = (x .&. s) > 0
        ry = (y .&. s) > 0
        rot = rotater s (x,y) rx ry
        inc = s * s * (number rx ry)

number :: Bool -> Bool -> Word32
number rx ry 
    | rx && ry       = 2
    | rx && (not ry) = 3
    | (not rx) && ry = 1
    | otherwise      = 0

rotater :: Word32 -> (Word32, Word32) -> Bool -> Bool -> (Word32, Word32)
rotater n (x,y) rx ry
    | rx && (not ry) = (n - 1 - x, n - 1- y)
    | (not ry)       = (y, x)
    | otherwise      = (x, y)
    
