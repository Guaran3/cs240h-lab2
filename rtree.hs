--this is the actual tree for the hilbert r-tree
--all that needs to be supported is insert and 
--search

import Rect
import Data.Maybe
import qualified Data.List as LS
import Data.Word


type MBR = Rect 
type LHV = Word32

leafmax  = 4
innermax = 4

data NodeType = Inner | Outer | Object | Empty deriving (Show, Eq)

data TNode = LNode LHV MBR [Rect]
           | INode LHV MBR [TNode]
           deriving (Show)

instance Eq TNode where
    (==) node1 node2 = (getLHV node1) == (getLHV node2)

instance Ord TNode where
    (<=) node1 node2 = (getLHV node1) <= (getLHV node2)


-- have to get the HV by hand without names
getLHV :: TNode -> LHV
getLHV (LNode a _ _) = a
getLHV (INode a _ _) = a

getMBR :: TNode -> MBR
getMBR (LNode _ a _) = a
getMBR (INode _ a _) = a


getNodes :: TNode -> [TNode]
getNodes (INode _ _ x) = x


-- adds rectangle to a node
addRect :: TNode -> Rect -> TNode
addRect (INode _ _ _) _ = error "must be a leaf node to work"
addRect (LNode large mbr list) rect = LNode (max large (hlbRect rect))
                                            (createMBR mbr rect)
                                            (LS.insert rect list)

-- inserts into tree
tinsert :: TNode -> Rect -> TNode
tinsert n@(LNode lhv mbr list) rect
    |(leafmax > (length list)) = refix $ (addRect n rect)
    |otherwise = refix $ INode (max lhv (hlbRect rect)) (createMBR mbr rect)
               [n, (LNode (hlbRect rect) rect [rect])]
tinsert n@(INode lhv mbr list) rect = refix $
    INode (max lhv (hlbRect rect)) (createMBR mbr rect)
          (LS.insert (tinsert cnode rect) (LS.delete cnode list))
    where
        cnode = rmJust $ LS.find (\elem -> (getLHV elem) > (hlbRect rect))
                              list
        rmJust (Just elem) = elem
        rmJust Nothing     = last list

-- fixes the changing possible LHV and MBR values, may not be needed
-- since there are no deletions
refix :: TNode -> TNode
refix (LNode lhv mbr list) = 
    LNode (max lhv $ maximum [hlbRect x | x <- list])
          (getMBR list)
          list
    where
        getMBR = foldl (\x y -> createMBR x y) mbr 
refix n@(INode lhv mbr list) 
--    | (length $ children list) < (length list -1) * leafmax = 
--        shuffle n
--    | otherwise =
       = INode lhv2 mbr2 list2
    where 
        lhv2 = foldl (\x y -> max x (getLHV y)) lhv list
        mbr2 = foldl (\x y -> createMBR x $ getMBR y) mbr list
        list2 = map refix list

--shuffle :: [TNode] -> TNode -> TNode
--shuffle ch@(x:xs) = 
children :: TNode -> [TNode]
children (INode _ _ list) = concat $ map getNodes list

{-
data Node = Node  { lhv    :: Word32
                  , ptr    :: Maybe [HTree]
                  , mbr    :: MBR
                  , nType  :: NodeType
                  } deriving (Show)

data HTree = HTree [Node] deriving (Show)

instance Eq Node where
    (==) h1 h2 = lhv h1 == lhv h2

instance Ord Node where
    (<=) h1 h2 = lhv h1 <= lhv h2

numChild :: Int
numChild = 4

numLeaf :: Int
numLeaf = 4
-}
--max number of intersecting rectangles.
maxNum :: Word16
maxNum = 4
{-
--returns a list of rectangles that intersect query window.
search :: HTree -> Rect -> [Rect]
search tree rect
    |(nType tree) == Empty  = error "trying to search in an empty list"
    |(nType tree) == Object = [(mbr tree)]
    |(nType tree) == Outer  = [mbr x | x <- ((fromJust . child) tree), (overlap . mbr) x]
    |(nType tree) == Inner  = concatMap (flip search rect) children
    where 
        overlap = intersect rect
        children = [x | x <-((fromJust . child) tree), (overlap . mbr) x]

-- creates a node to contain an actual rectangle 
wrapRect :: Rect -> HTree
wrapRect rect = Node { lhv   = hlbRect rect
                     , child = Nothing
                     , mbr   = rect
                     , nType = Object
                     }



--inserts a rectangle into the tree
insert :: HTree -> Rect -> HTree
insert root rect
    |

-- find LHV from a given list of HTrees, presumably children
getLHV :: [Node] -> Word32
getLHV list = maximum [lhv x | x <- list]

-- fine the MBR for a given list of HTrees, again
-- presumably children
getMBR :: [Node] -> MBR
getMBR (a:[]) = mbr a
getMBR (a:as) = createMBR (mbr a) (getMBR as)
-}



