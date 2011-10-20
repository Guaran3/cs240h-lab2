--this is the actual tree for the hilbert r-tree
--all that needs to be supported is insert and 
--search

import Rect
import Data.List

type MBR = Rect

data HTree = Node { lhv   :: Word32
                  , child :: [HTree]
                  , mbr   :: MBR
                  } deriving (Show)

instance Eq HTree where
    (==) h1 h2 = lhv h1 == lhv h2

instance Ord HTree where
    (<=) h1 h2 = lhv h1 <= lhv h2

numEntries :: Word16
numEntries = 2

--max number of intersecting rectangles.
maxNum :: Word16
maxNum = 4

--returns a list of rectangles that intersect query window.
search :: HTree -> Rect -> [Rect]


--inserts a rectangle into the tree
insert :: HTree -> Rect -> HTree

-- find LHV from a given list of HTrees, presumably children
getLHV :: [HTree] -> Word32
getLHV list = maximum [lhv x | x <- list

-- fine the MBR for a given list of HTrees, again
-- presumably children
getMBR :: [HTree] -> MBR
getMBR (a:[]) = mbr a
getMBR (a:as) = createMBR a (getMBR as)





