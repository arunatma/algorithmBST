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
