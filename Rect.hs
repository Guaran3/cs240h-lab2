-- This is the structure for rectangle
module Rect( Rect(..)
           , hlbRect
           , createRect
           , intersect
           , createMBR
           ) where

import Data.Word
import Data.Bits
import Test.QuickCheck
import HilbertFunction

type Point = (Word32, Word32)
-- unlike the data inputs, we should only need 4 datapoints
-- the lower and upper bounds of x and y
data Rect = Rect { xMin :: Word32
                 , yMin :: Word32
                 , xMax :: Word32
                 , yMax :: Word32
                 } deriving (Show)

-- creating a test for equality for Rects
instance Eq Rect where
    (==) r1 r2 = xMin r1 == xMin r2 &&
                 yMin r1 == yMin r2 &&
                 xMax r1 == xMax r2 &&
                 yMax r1 == yMax r2

--creates MBR of a given quadrilateral from the file
createRect :: [Int] -> Rect
createRect [x1,y1,x2,y2,x3,y3,x4,y4] = Rect xmin ymin xmax ymax where
    xx = [x1,x2,x3,x4]
    yy = [y1,y2,y3,y4]
    xmin = fromIntegral (minimum xx) :: Word32
    ymin = fromIntegral (minimum yy) :: Word32
    xmax = fromIntegral (maximum xx) :: Word32
    ymax = fromIntegral (maximum yy) :: Word32

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
hlbRect :: Rect -> Word32
hlbRect = hlb . center where
    hlb (x,y) = hilbert x y
