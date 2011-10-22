--this is the actual tree for the hilbert r-tree
--all that needs to be supported is insert and 
--search

import qualified Rect as R
import qualified Data.List as LS
import System.CPUTime
import System.Environment
import qualified Data.Text as TXT

main :: IO ()
main = do
    args  <- getArgs
    start <- getCPUTime
    raw   <- readFile $ head args
    let rects = toRects raw
    let tree  = refix $ foldl tinsert (LNode 0 (head rects) []) rects
    end   <- getCPUTime
    let time  = fromIntegral (end - start) / fromIntegral ((10 :: Int) ^ (9 :: Int))
    putStrLn $ (show $ length rects) ++ " rects read in " ++ (show (time::Double)) ++ "ms."

    stext <- getContents
    let srect = toRects stext
    
    showMatch tree srect

showMatch :: TNode -> [R.Rect] -> IO ()
showMatch tree list = do
    start <- getCPUTime
    let nlist = take maxout (concat $ map (search tree) list)
    end   <- getCPUTime
    let time  = fromIntegral (end - start) / fromIntegral ((10 :: Int) ^ (9 :: Int))
    putStrLn $ (show $ length nlist) ++ " rects found in " ++ (show (time::Double)) ++ "ms."
    printAll nlist 

--stolen from online site :p (iceteks.com)
printAll :: [R.Rect] -> IO ()
printAll []     = return ()
printAll (x:xs) = do print x
                     printAll xs

--turns a string from file into rectangles
toRects :: String -> [R.Rect]
toRects inp = [R.createRect x | x <- flist]
    where 
        list  = TXT.lines $ TXT.pack inp
        flist = [map (\x -> read $ TXT.unpack x) (TXT.splitOn (TXT.pack ",") line)
                | line <- list]

type MBR = R.Rect 
type LHV = Int

maxout :: Int
maxout   = 5

leafmax :: Int
leafmax  = 4

--innermax :: Int
--innermax = 4

data NodeType = Inner | Outer | Object | Empty deriving (Show, Eq)

data TNode = LNode LHV MBR [R.Rect]
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


--getNodes :: TNode -> [TNode]
--getNodes (INode _ _ x) = x


-- adds rectangle to a node
addRect :: TNode -> R.Rect -> TNode
addRect (INode _ _ _) _ = error "must be a leaf node to work"
addRect (LNode large mbr list) rect = LNode (max large (R.hlbRect rect))
                                            (R.createMBR mbr rect)
                                            (LS.insert rect list)

-- inserts into tree
tinsert :: TNode -> R.Rect -> TNode
tinsert n@(LNode lhv mbr list) rect
    |(leafmax > (length list)) = refix $ (addRect n rect)
    |otherwise = refix $ INode lhv mbr 
               [n, (LNode (R.hlbRect rect) rect [rect])]
tinsert (INode lhv mbr list) rect = refix $
    INode lhv mbr (LS.insert (tinsert cnode rect) (LS.delete cnode list))
    where
        cnode = rmJust $ LS.find (\el -> (getLHV el) > (R.hlbRect rect))
                              list
        rmJust (Just el) = el
        rmJust Nothing     = last list

-- fixes the changing possible LHV and MBR values, may not be needed
-- since there are no deletions
refix :: TNode -> TNode
refix (LNode lhv mbr list) = 
    LNode (max lhv $ maximum [R.hlbRect x | x <- list])
          (getMBRList list)
          list
    where
        getMBRList = foldl (\x y -> R.createMBR x y) mbr 
refix (INode lhv mbr list) 
--    this part should be doing the s to s+1 merging stuff, not working right now
--    | (length $ children list) < (length list -1) * leafmax = 
--        shuffle n
--    | otherwise =
       = INode lhv2 mbr2 list2
    where 
        lhv2 = foldl (\x y -> max x (getLHV y)) lhv list
        mbr2 = foldl (\x y -> R.createMBR x $ getMBR y) mbr list
        list2 = map refix list

--shuffle :: [TNode] -> TNode -> TNode
--shuffle ch@(x:xs) = 
--children :: TNode -> [TNode]
--children (INode _ _ list) = concat $ map getNodes list

--returns search list
search :: TNode -> R.Rect -> [R.Rect]
search (LNode _ _ list) rect = filter (\r2 -> R.intersect rect r2) list
search (INode _ _ list) rect = concat $ (map $ flip search rect) ( 
    filter (\node -> R.intersect rect $ getMBR node) list ) 


