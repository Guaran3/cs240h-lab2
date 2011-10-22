-- functions to convert from xy coordinates to hilbert, and
-- from hilber to x,y.


module HilbertFunction(hilbert, hilbertFn) where

-- trying new method, pretty much the same as the one on O'sullivan's blog
-- since I think I was getting pretty weird values for the method above
-- note output is Word32 since hilbert curve is one to one, and there are 
-- (2^16)^2 points in the grid we are using

hilbertFn :: Int -> Int -> Int -> Int
hilbertFn size x y 
    | x < 0 || x >= size = error "x isn't in range"
    | y < 0 || y >= size = error "y isn't in range"
    | otherwise = distance csize 0 x y where
        csize = size `div` 2
        --print csize
        distance 0 result _ _ = result 
        distance side result x1 y1 =
            case (compare x1 side, compare y1 side) of 
                -- bottom left, don't add extra area
                -- flip x,y since bottom corner reverses every iteration
                (LT, LT) -> step result y1 x1 
                -- upper left, add 1x area (the first quadrant)
                -- don't flip, recenter though since 2nd part never rotates
                (LT, _ ) -> step (result + side^(2::Int)) x1 (y1 - side)
                -- lower right, add 3x (last section)
                -- 180 deg rotation, translation to center, then flip like first one
                ( _, LT) -> step (result + 3*side^(2::Int))
                            (side - y1 -1) (2*side - x1 - 1) 
                -- upper rigth, add 2x distance
                -- also just translation
                ( _, _ ) -> step (result + 2*side^(2::Int)) 
                            (x1 - side) (y1 - side)
                where step = distance (side `div` 2)

hilbert :: Int -> Int -> Int
hilbert = hilbertFn 65536




