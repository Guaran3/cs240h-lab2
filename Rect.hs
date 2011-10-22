-- This is the structure for rectangle
module Rect( Rect(..)
           , hlbRect
           , createRect
           , intersect
           , createMBR
           ) where

import HilbertFunction

type Point = (Int, Int)
-- unlike the data inputs, we should only need 4 datapoints
-- the lower and upper bounds of x and y
data Rect = Rect { xMin :: Int
                 , yMin :: Int
                 , xMax :: Int
                 , yMax :: Int
                 } deriving (Show)


-- creating a test for equality for Rects
instance Eq Rect where
    (==) r1 r2 = xMin r1 == xMin r2 &&
                 yMin r1 == yMin r2 &&
                 xMax r1 == xMax r2 &&
                 yMax r1 == yMax r2

instance Ord Rect where
    compare r1 r2
        | hlbRect r1 > hlbRect r2 = GT
        | hlbRect r1 < hlbRect r2 = LT
        | otherwise               = EQ
--creates MBR of a given quadrilateral from the file
createRect :: [Int] -> Rect
createRect [x1,y1,x2,y2,x3,y3,x4,y4] = Rect xmin ymin xmax ymax where
    xx = [x1,x2,x3,x4]
    yy = [y1,y2,y3,y4]
    xmin = fromIntegral (minimum xx) :: Int
    ymin = fromIntegral (minimum yy) :: Int
    xmax = fromIntegral (maximum xx) :: Int
    ymax = fromIntegral (maximum yy) :: Int
createRect _ = error "dimensions are incorrect"

--finde the center of the rectangle
center :: Rect -> Point
center rect =  (avgx, avgy) where
    x1 = xMin rect
    x2 = xMax rect
    y1 = yMin rect
    y2 = yMax rect
    avgx = (x1 + ((x2 - x1) `div` 2))
    avgy = (y1 + ((y2 - y1) `div` 2))

--check to see if two rectangles intersect:
intersect :: Rect -> Rect -> Bool
intersect r1 r2 = not (xMin r1 > xMax r2 ||
                       xMax r1 < xMin r2 ||
                       yMin r1 > yMax r2 ||
                       yMax r1 < yMin r2 )

--create a MBR for 2 rectangles, note that this can
-- be folded to get an MBR for more than 2 Rects
createMBR :: Rect -> Rect -> Rect
createMBR r1 r2 = 
    Rect {
        xMin = min (xMin r1) (xMin r2) ,
        yMin = min (yMin r1) (yMin r2) ,
        xMax = max (xMax r1) (xMax r2) ,
        yMax = max (yMax r1) (yMax r2) }

--get the hilbert value for a particular rectangle.  
hlbRect :: Rect -> Int
hlbRect = hlb . center where
    hlb (x,y) = hilbert x y
