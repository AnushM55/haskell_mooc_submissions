-- Exercise set 5b: playing with binary trees

module Set5b where

import Mooc.Todo
import Data.Maybe

-- The next exercises use the binary tree type defined like this:

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- Ex 1: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Empty)

valAtRoot :: Tree a -> Maybe a
valAtRoot Empty = Nothing
valAtRoot (Node a _ _) = Just a 

------------------------------------------------------------------------------
-- Ex 2: compute the size of a tree, that is, the number of Node
-- constructors in it
--
-- Examples:
--   treeSize (Node 3 (Node 7 Empty Empty) Empty)  ==>  2
--   treeSize (Node 3 (Node 7 Empty Empty) (Node 1 Empty Empty))  ==>  3

treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node _ tree1 tree2) = 1 + (treeSize tree1) + (treeSize tree2)

------------------------------------------------------------------------------
-- Ex 3: get the largest value in a tree of positive Ints. The
-- largest value of an empty tree should be 0.
--
-- Examples:
--   treeMax Empty  ==>  0
--   treeMax (Node 3 (Node 5 Empty Empty) (Node 4 Empty Empty))  ==>  5

treeMax :: Tree Int -> Int
treeMax Empty = 0
treeMax (Node val tree1 tree2)  = max val (max (treeMax tree1) (treeMax tree2))
------------------------------------------------------------------------------
-- Ex 4: implement a function that checks if all tree values satisfy a
-- condition.
--
-- Examples:
--   allValues (>0) Empty  ==>  True
--   allValues (>0) (Node 1 Empty (Node 2 Empty Empty))  ==>  True
--   allValues (>0) (Node 1 Empty (Node 0 Empty Empty))  ==>  False

allValues :: (a -> Bool) -> Tree a -> Bool
allValues _ Empty = True
allValues condition (Node val tree1 tree2) = (condition val ) && (allValues condition tree1) && (allValues condition tree2)

------------------------------------------------------------------------------
-- Ex 5: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Empty  ==>  Empty
-- mapTree (+2) (Node 0 (Node 1 Empty Empty) (Node 2 Empty Empty))
--   ==> (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node val tree1 tree2) = Node (f val) (mapTree f tree1) (mapTree f tree2)
------------------------------------------------------------------------------
-- Ex 6: given a value and a tree, build a new tree that is the same,
-- except all nodes that contain the value have been removed. Also
-- remove the subnodes of the removed nodes.
--
-- Examples:
--
--     1          1
--    / \   ==>    \
--   2   0          0
--
--  cull 2 (Node 1 (Node 2 Empty Empty)
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty))
--
--      1           1
--     / \           \
--    2   0   ==>     0
--   / \
--  3   4
--
--  cull 2 (Node 1 (Node 2 (Node 3 Empty Empty)
--                         (Node 4 Empty Empty))
--                 (Node 0 Empty Empty))
--     ==> (Node 1 Empty
--                 (Node 0 Empty Empty)
--
--    1              1
--   / \              \
--  0   3    ==>       3
--   \   \
--    2   0
--
--  cull 0 (Node 1 (Node 0 Empty
--                         (Node 2 Empty Empty))
--                 (Node 3 Empty
--                         (Node 0 Empty Empty)))
--     ==> (Node 1 Empty
--                 (Node 3 Empty Empty))

cull :: Eq a => a -> Tree a -> Tree a
cull _ Empty = Empty
cull target (Node val tree1 tree2)= case (val == target) of True -> Empty
                                                            False -> (Node val (cull target tree1) (cull target tree2)) 

------------------------------------------------------------------------------
-- Ex 7: check if a tree is ordered. A tree is ordered if:
--  * all values to the left of the root are smaller than the root value
--  * all of the values to the right of the root are larger than the root value
--  * and the left and right subtrees are ordered.
--
-- Hint: allValues will help you here!
--
-- Examples:
--         1
--        / \   is ordered:
--       0   2
--   isOrdered (Node 1 (Node 0 Empty Empty)
--                     (Node 2 Empty Empty))   ==>   True
--
--         1
--        / \   is not ordered:
--       2   3
--   isOrdered (Node 1 (Node 2 Empty Empty)
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        1     3   is not ordered:
--         \
--          0
--   isOrdered (Node 2 (Node 1 Empty
--                             (Node 0 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   False
--
--           2
--         /   \
--        0     3   is ordered:
--         \
--          1
--   isOrdered (Node 2 (Node 0 Empty
--                             (Node 1 Empty Empty))
--                     (Node 3 Empty Empty))   ==>   True

isOrdered :: Ord a => Tree a -> Bool
isOrdered Empty = True
isOrdered (Node val tree1 tree2) = (allValues (< val) tree1) &&
                                   (allValues (> val) tree2) &&
                                   (isOrdered tree1) && (isOrdered tree2)

------------------------------------------------------------------------------
-- Ex 8: a path in a tree can be represented as a list of steps that
-- go either left or right.

data Step = StepL | StepR
  deriving (Show, Eq)

-- Define a function walk that takes a tree and a list of steps, and
-- returns the value at that point. Return Nothing if you fall of the
-- tree (i.e. hit a Empty).
--
-- Examples:
--   walk [] (Node 1 (Node 2 Empty Empty) Empty)       ==>  Just 1
--   walk [StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Just 2
--   walk [StepL,StepL] (Node 1 (Node 2 Empty Empty) Empty)  ==>  Nothing

walk :: [Step] -> Tree a -> Maybe a
walk _ Empty = Nothing
walk [] (Node val _ _) = Just val
walk (fs:rest) (Node val tree1 tree2) = case fs of StepL -> walk rest tree1
                                                   StepR -> walk rest tree2

------------------------------------------------------------------------------
-- Ex 9: given a tree, a path and a value, set the value at the end of
-- the path to the given value. Since Haskell datastructures are
-- immutable, you'll need to build a new tree.
--
-- If the path falls off the tree, do nothing.
--
-- Examples:
--   set [] 1 (Node 0 Empty Empty)  ==>  (Node 1 Empty Empty)
--   set [StepL,StepL] 1 (Node 0 (Node 0 (Node 0 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--                  ==>  (Node 0 (Node 0 (Node 1 Empty Empty)
--                                       (Node 0 Empty Empty))
--                               (Node 0 Empty Empty))
--
--   set [StepL,StepR] 1 (Node 0 Empty Empty)  ==>  (Node 0 Empty Empty)

set :: [Step] -> a -> Tree a -> Tree a
set _ _ Empty = Empty
set [] target (Node val tree1 tree2)  = (Node target tree1 tree2)
set (fs:xs) target (Node val tree1 tree2) = case fs of StepL -> (Node val (set xs target tree1) tree2)
                                                       StepR -> (Node val tree1 (set xs target tree2))


------------------------------------------------------------------------------
-- Ex 10: given a value and a tree, return a path that goes from the
-- root to the value. If the value doesn't exist in the tree, return Nothing.
--
-- You may assume the value occurs in the tree at most once.
--
-- Examples:
--   search 1 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))  ==>  Just [StepL]
--   search 1 (Node 2 (Node 4 Empty Empty) (Node 3 Empty Empty))  ==>  Nothing
--   search 1 (Node 2 (Node 3 (Node 4 Empty Empty)
--                            (Node 1 Empty Empty))
--                    (Node 5 Empty Empty))                     ==>  Just [StepL,StepR]

search :: Eq a => a -> Tree a -> Maybe [Step]
search _ Empty = Nothing
search target (Node val Empty Empty ) = case (target == val) of False -> Nothing
                                                                True -> Just []
search target (Node val tree1 tree2) = case (target == val) of True -> Just []
                                                               False -> (case (search target tree1) of Nothing -> (case (search target tree2) of Nothing -> Nothing
                                                                                                                                                 Just list -> (Just (StepR : list))) 
                                                                                                       Just list -> (Just (StepL : list)))
