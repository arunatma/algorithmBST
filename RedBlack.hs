-- Plan: 
-- Step 1: Simple BST
-- Step 2: 2-3 BST (balanced)
-- Step 3: Red Black BST (balanced)  
-- Since 2 and 3 are same, next check point from 1 should be 3

-- References:
-- http://learnyouahaskell.com/zippers
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
-- Prof. Robert Sedgewick "Algorithms 4th Edition" 
-- http://algs4.cs.princeton.edu/30searching/

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  
    
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  

-- finds the minimum node tree                           
treeMin :: (Ord a) => Tree a -> Tree a 
treeMin ft@(Node x EmptyTree _) = ft
treeMin (Node x left _) = treeMin left

deleteMin :: Tree a -> Tree a
deleteMin (Node x EmptyTree right) = right
deleteMin (Node x left right) = (Node x (deleteMin left) right) 

delete :: (Ord a) => a -> Tree a -> Tree a
delete k EmptyTree = EmptyTree
delete k (Node x left right) 
    | k < x = Node x (delete k left) right
    | k > x = Node x left (delete k right)
    | right == EmptyTree = left
    | left == EmptyTree = right
    | otherwise = Node minR left (deleteMin right) 
        where Node minR lt rt = (treeMin right)

        
-- Example
{-
        10  15 3 20 34 21 5 6 8 4 3 9
        
                    10
                  3      15
                    5        20 
                   4  6           34 
                        8      21 
                          9
-}


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

data Color = Red | Black deriving (Show, Read, Eq)

data TreeRBT a = EmptyRBT | NodeRBT a Color (TreeRBT a) (TreeRBT a) deriving (Show, Read, Eq)

red :: (Ord a) => TreeRBT a -> Bool
red EmptyRBT            = False
red (NodeRBT _ c _ _)   = c == Red

black :: (Ord a) => TreeRBT a -> Bool
black = not . red

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
insertRBT x t@(NodeRBT a c l r)
    | red nr && black nl    = rotateLeft nt
    | red nl && red nll     = rotateRight nt
    | red nl && red nr      = flipColors nt
    | otherwise             = nt
    where nl                      = newL x t 
          nr                      = newR x t
          nt                      = newT x t
          (NodeRBT nx nc nll nlr) = nl
    
newT :: (Ord a) => a -> TreeRBT a -> TreeRBT a    
newT x EmptyRBT = singleRBT x
newT x (NodeRBT a c l r)
    | x < a     = NodeRBT a c (insertRBT x l) r
    | x > a     = NodeRBT a c l (insertRBT x r)
    | x == a    = NodeRBT a c l r
    
newL :: (Ord a) => a -> TreeRBT a -> TreeRBT a    
newL x EmptyRBT = singleRBT x
newL x (NodeRBT a c l r)
    | x < a     = insertRBT x l
    | otherwise = l

newR :: (Ord a) => a -> TreeRBT a -> TreeRBT a    
newR x EmptyRBT = singleRBT x
newR x (NodeRBT a c l r)
    | x > a     = insertRBT x r
    | otherwise = r
    
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
searchRBT x (NodeRBT a c left right)  
    | x == a = True  
    | x < a  = searchRBT x left  
    | x > a  = searchRBT x right
    
    