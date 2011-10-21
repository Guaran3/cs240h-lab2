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
    |otherwise = refix $ INode lhv mbr 
               [n, (LNode (hlbRect rect) rect [rect])]
tinsert n@(INode lhv mbr list) rect = refix $
    INode lhv mbr (LS.insert (tinsert cnode rect) (LS.delete cnode list))
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
--    this part should be doing the s to s+1 merging stuff, not working right now
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

--returns search list
search :: TNode -> Rect -> [Rect]
search (LNode _ _ list) rect = filter (\r2 -> intersect rect r2) list
search (INode _ _ list) rect = concat $ (map $ flip search rect) ( 
    filter (\node -> intersect rect $ getMBR node) list ) 


