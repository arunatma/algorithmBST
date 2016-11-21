
import Test.QuickCheck

-- Data Structure to hold a normal tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a l r)   
    | x == a = Node x l r  
    | x < a  = Node a (treeInsert x l) r  
    | x > a  = Node a l (treeInsert x r)  
    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a l r)  
    | x == a = True  
    | x < a  = treeElem x l  
    | x > a  = treeElem x r  

-- finds the minimum node tree                           
treeMin :: (Ord a) => Tree a -> Tree a 
treeMin ft@(Node x EmptyTree _) = ft
treeMin (Node x l _) = treeMin l

deleteMin :: Tree a -> Tree a
deleteMin (Node x EmptyTree r) = r
deleteMin (Node x l r) = (Node x (deleteMin l) r) 

delete :: (Ord a) => a -> Tree a -> Tree a
delete k EmptyTree = EmptyTree
delete k (Node x l r) 
    | k < x = Node x (delete k l) r
    | k > x = Node x l (delete k r)
    | r == EmptyTree = l
    | l == EmptyTree = r
    | otherwise = Node minR l (deleteMin r) 
        where Node minR lt rt = (treeMin r)
        
data Color = Red | Black deriving (Show, Read, Eq)

data TreeRBT a = EmptyRBT | NodeRBT a Color (TreeRBT a) (TreeRBT a) deriving (Show, Read, Eq)

isRed :: (Ord a) => TreeRBT a -> Bool
isRed EmptyRBT            = False
isRed (NodeRBT _ c _ _)   = c == Red

isBlack :: (Ord a) => TreeRBT a -> Bool
isBlack = not . isRed

rotateLeft :: (Ord a) => TreeRBT a -> TreeRBT a
rotateLeft EmptyRBT                                 = EmptyRBT
rotateLeft orig@(NodeRBT _ _ _ EmptyRBT)            = orig
rotateLeft (NodeRBT a c l (NodeRBT ra rc rl rr))    = 
    NodeRBT ra c (NodeRBT a Red l rl) rr

rotateRight :: (Ord a) => TreeRBT a -> TreeRBT a
rotateRight EmptyRBT                                 = EmptyRBT
rotateRight orig@(NodeRBT _ _ EmptyRBT _)            = orig
rotateRight (NodeRBT a c (NodeRBT la lc ll lr) r)   = 
    NodeRBT la c ll (NodeRBT a Red lr r)
    
singleRBT :: a -> TreeRBT a  
singleRBT x = NodeRBT x Red EmptyRBT EmptyRBT

insertRBT :: (Ord a) => a -> TreeRBT a -> TreeRBT a    
insertRBT x EmptyRBT = singleRBT x
insertRBT x inTree@(NodeRBT a c l r)
    | x < a     = realign $ NodeRBT a c (insertRBT x l) r
    | x > a     = realign $ NodeRBT a c l (insertRBT x r)
    | x == a    = inTree
    where 
    realign t = step3 $ step2 $ step1 t 
    step1 t1@(NodeRBT a1 c1 l1 r1)
        | isRed r1 && isBlack l1    = rotateLeft t1
        | otherwise                 = t1
    step2 t2@(NodeRBT a2 c2 l2 r2)
        | isRed l2 && (isRed . left $ l2)   = rotateRight t2
        | otherwise                         = t2
    step3 t3@(NodeRBT a2 c2 l2 r2)
        | isRed l2 && isRed r2      = flipColors t3
        | otherwise                 = t3
    
invert :: Color -> Color
invert Red = Black
invert Black = Red 
    
changeColor :: (Ord a) => TreeRBT a -> TreeRBT a
changeColor EmptyRBT = EmptyRBT
changeColor (NodeRBT a c l r) = NodeRBT a (invert c) l r
    
flipColors :: (Ord a) => TreeRBT a -> TreeRBT a
flipColors EmptyRBT = EmptyRBT
flipColors (NodeRBT a c l r) = NodeRBT a (invert c) nl nr 
    where nl = changeColor l 
          nr = changeColor r
          
searchRBT :: (Ord a) => a -> TreeRBT a -> Bool  
searchRBT x EmptyRBT = False  
searchRBT x (NodeRBT a c l r)  
    | x == a = True  
    | x < a  = searchRBT x l  
    | x > a  = searchRBT x r
    
left :: (Ord a) => TreeRBT a -> TreeRBT a
left EmptyRBT = EmptyRBT
left (NodeRBT a c l r) = l

right :: (Ord a) => TreeRBT a -> TreeRBT a
right EmptyRBT = EmptyRBT
right (NodeRBT a c l r) = r

moveRedLeft :: (Ord a) => TreeRBT a -> TreeRBT a
moveRedLeft t = procMoveLeft (flipColors t)
    where procMoveLeft EmptyRBT = EmptyRBT
          procMoveLeft t 
            | isRed . left . right $ t  =  flipColors . rotateLeft . rotateRightBranch $ t
            | otherwise                 = t
    
rotateRightBranch :: (Ord a) => TreeRBT a -> TreeRBT a
rotateRightBranch  EmptyRBT         = EmptyRBT    
rotateRightBranch (NodeRBT a c l r) = NodeRBT a c l (rotateRight r)

moveRedRight :: (Ord a) => TreeRBT a -> TreeRBT a
moveRedRight t = procMoveRight (flipColors t)
    where procMoveRight EmptyRBT = EmptyRBT
          procMoveRight t 
            | isRed . left . left $ t  =  flipColors . rotateRight $ t
            | otherwise                 = t
            
balance :: (Ord a) => TreeRBT a -> TreeRBT a
balance EmptyRBT = EmptyRBT
balance t
    | isRed . right $ t                                 = rotateLeft t
    | (isRed . left $ t) && (isRed . left . left $ t)   = rotateRight t
    | (isRed . left $ t) && (isRed . right $ t)         = flipColors t
    | otherwise                                         = t

isEmptyRBT :: (Ord a) => TreeRBT a -> Bool
isEmptyRBT t = (t == EmptyRBT)
    
deleteMinRBT :: (Ord a) => TreeRBT a -> TreeRBT a
deleteMinRBT EmptyRBT = EmptyRBT
deleteMinRBT t 
    | isEmptyRBT . left $ t                                 = EmptyRBT
    | (isBlack . left $ t) && (isBlack. left . left $ t)    = postProc (moveRedLeft t)
    | otherwise                                             = postProc t
    where 
    postProc t1@(NodeRBT a c l r) = balance (NodeRBT a c (deleteMinRBT l) r)
    
deleteRBT :: (Ord a) => a -> TreeRBT a -> TreeRBT a
deleteRBT k EmptyRBT = EmptyRBT
deleteRBT k t@(NodeRBT a c l r)
    | k < a = balance $ deleteFromLeft (preProc t)
    | otherwise = balance $ deleteFromRight t
    where deleteFromLeft t1@(NodeRBT a1 c1 l1 r1) =  (NodeRBT a1 c1 (deleteRBT k l1) r1)
          preProc t2
            | (isBlack . left $ t2) && (isBlack. left . left $ t2) = moveRedLeft t2
            | otherwise                                            = t2 
          deleteFromRight t3 = cond4 . cond3 . cond2. cond1 $ t3
          cond1 x 
            | isRed (left x)    = rotateRight x
            | otherwise         = x
          cond2 EmptyRBT    = EmptyRBT
          cond2 x@(NodeRBT a2 c2 l2 r2) 
            | (k == a2) && (isEmptyRBT . right $ x)  = EmptyRBT
            | otherwise                              = x
          cond3 x
            | (isBlack . right $ x) && (isBlack . left . right $ x)    = moveRedRight x
            | otherwise                                                = x
          cond4 EmptyRBT   = EmptyRBT
          cond4 x@(NodeRBT a4 c4 l4 r4) 
            | (k == a4) = NodeRBT (minKey r4) c4 l4 (deleteMinRBT r4)
            | otherwise = NodeRBT a4 c4 l4 (deleteRBT k r4)
          minKey x = getKey $ minRBT x
          getKey x@(NodeRBT a5 _ _ _) = a5   
          
minRBT :: (Ord a) => TreeRBT a -> TreeRBT a
minRBT EmptyRBT = EmptyRBT
minRBT t@(NodeRBT a c l r) 
    | l == EmptyRBT = t
    | otherwise     = minRBT l


freeTree :: Tree Int  
freeTree =   
    Node 10  
        (Node 3  
            (EmptyTree)  
            (Node 5  
                (Node 4 EmptyTree EmptyTree)  
                (Node 6 
                    (EmptyTree) 
                    (Node 8 
                        (EmptyTree)
                        (Node 9 EmptyTree EmptyTree)
                    )
                )  
            )  
        )  
        (Node 15  
            (EmptyTree)  
            (Node 20
                (EmptyTree)
                (Node 34 
                    (Node 21 EmptyTree EmptyTree)
                    (EmptyTree)  
                )
            )  
        ) 
    
atojRBT = insertRBT 'J' $ insertRBT 'I' $ insertRBT 'H' $ insertRBT 'G' $ insertRBT 'F' $ insertRBT 'E' $ insertRBT 'D' $ insertRBT 'C' $ insertRBT 'B' $ insertRBT 'A' EmptyRBT

jtoaRBT = insertRBT 'A' $ insertRBT 'B' $ insertRBT 'C' $ insertRBT 'D' $ insertRBT 'E' $ insertRBT 'F' $ insertRBT 'G' $ insertRBT 'H' $ insertRBT 'I' $ insertRBT 'J' EmptyRBT

-- QuickCheck Tests 
-- QuickCheck References
-- http://www.stuartgunter.org/intro-to-quickcheck/
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

test1 = quickCheck ((\x -> (deleteMinRBT (singleRBT x)) == EmptyRBT) :: Char -> Bool)
test2 = quickCheck ((\x -> (deleteMinRBT (singleRBT x)) == EmptyRBT) :: Int -> Bool)
test3 = quickCheck ((\x -> (deleteMinRBT (insertRBT x EmptyRBT)) == EmptyRBT) :: Char -> Bool)
test4 = quickCheck ((\x -> (deleteMinRBT (insertRBT x EmptyRBT)) == EmptyRBT) :: Int -> Bool)

test5 = quickCheck ((\x y -> (deleteMinRBT (deleteMinRBT (insertRBT y (insertRBT x EmptyRBT)))) == EmptyRBT) :: Char -> Char -> Bool)
test6 = quickCheck ((\x y -> (deleteMinRBT (deleteMinRBT (insertRBT y (insertRBT x EmptyRBT)))) == EmptyRBT) :: Int -> Int -> Bool)

test7 = quickCheck ((\x y z -> (deleteRBT y (deleteRBT z (deleteRBT x  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)
test8 = quickCheck ((\x y z -> (deleteRBT z (deleteRBT x (deleteRBT y  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)
test9 = quickCheck ((\x y z -> (deleteRBT x (deleteRBT y (deleteRBT z  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)
test10 = quickCheck ((\x y z -> (deleteRBT z (deleteRBT y (deleteRBT x  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)
test11 = quickCheck ((\x y z -> (deleteRBT x (deleteRBT z (deleteRBT y  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)
test12 = quickCheck ((\x y z -> (deleteRBT y (deleteRBT x (deleteRBT z  
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT)))))) == EmptyRBT) :: Char -> Char -> Char -> Bool)

test13 = quickCheck ((\x y z -> (deleteRBT y (deleteRBT z   
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT))))) == singleRBT x) :: Char -> Char -> Char -> Bool)
test14 = quickCheck ((\x y z -> (deleteRBT x (deleteRBT z   
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT))))) == singleRBT y) :: Char -> Char -> Char -> Bool)                          
test15 = quickCheck ((\x y z -> (deleteRBT x (deleteRBT y   
                               (insertRBT z (insertRBT y (insertRBT x EmptyRBT))))) == singleRBT z) :: Char -> Char -> Char -> Bool)

-- test16 = quickCheck ((\x -> (searchRBT x jtoaRBT) == True) :: Char -> Bool)

-- References:
-- http://learnyouahaskell.com/zippers
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
-- Prof. Robert Sedgewick "Algorithms 4th Edition" 
-- http://algs4.cs.princeton.edu/30searching/
-- Visualization: https://www.cs.usfca.edu/~galles/visualization/RedBlack.html
-- D3 Algorithms: https://bost.ocks.org/mike/algorithms/