-- functions to convert from xy coordinates to hilbert, and
-- from hilber to x,y.


module HilbertFunction(xy2d, hilbert, hilbertFn) where

import Data.Word
import Data.Bits
import System.IO

-- converts x and y coordinates to the corresponding hilbert value,
-- stolen almost directly from the wikipedia page
xy2d :: Word16 -> Word16 -> Word16 -> Word32 
xy2d size x y = xy2dr size (x, y) 0 where
    xy2dr 0 (x, y) total = total :: Word32
    xy2dr s (x, y) total = xy2dr (s `div` 2) rot (total + inc) where
        rx = (x .&. s) > 0 :: Bool
        ry = (y .&. s) > 0 :: Bool
        rot = rotater s (x,y) rx ry
        s2 = fromIntegral s :: Word32
        inc = s2 * s2 * (number rx ry) :: Word32

-- just to turn the True/False into 1/0
number :: Bool -> Bool -> Word32
number rx ry 
    | rx && ry       = 2 :: Word32
    | rx && (not ry) = 3 :: Word32
    | (not rx) && ry = 1 :: Word32
    | otherwise      = 0 :: Word32

--again, same as the wikipedia page used to rotate the fractal
--the right way
rotater :: Word16 -> (Word16, Word16) -> Bool -> Bool -> (Word16, Word16)
rotater n (x,y) rx ry
    | rx && (not ry) = (n - 1 - x, n - 1- y)
    | (not ry)       = (y, x)
    | otherwise      = (x, y)

-- trying new method, pretty much the same as the one on O'sullivan's blog
-- since I think I was getting pretty weird values for the method above
-- note output is Word32 since hilbert curve is one to one, and there are 
-- (2^16)^2 points in the grid we are using

hilbertFn :: Word32 -> Word32 -> Word32 -> Word32
hilbertFn size x y 
    | x < 0 || x >= size = error "x isn't in range"
    | y < 0 || y >= size = error "y isn't in range"
    | otherwise = distance csize 0 x y where
        csize = size `div` 2
        --print csize
        distance 0 result _ _ = result 
        distance side result x y =
            case (compare x side, compare y side) of 
                -- bottom left, don't add extra area
                -- flip x,y since bottom corner reverses every iteration
                (LT, LT) -> step result y x 
                -- upper left, add 1x area (the first quadrant)
                -- don't flip, recenter though since 2nd part never rotates
                (LT, _ ) -> step (result + side^2) x (y - side)
                -- lower right, add 3x (last section)
                -- 180 deg rotation, translation to center, then flip like first one
                ( _, LT) -> step (result + 3*side^2)
                            (side - y -1) (2*side - x - 1) 
                -- upper rigth, add 2x distance
                -- also just translation
                ( _, _ ) -> step (result + 2*side^2) 
                            (x - side) (y - side)
                where step = distance (side `div` 2)


hilbert = hilbertFn 65536




