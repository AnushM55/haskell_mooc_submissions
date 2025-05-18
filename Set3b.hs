-- Exercise set 3b
--
-- This is a special exercise set. The exercises are about
-- implementing list functions using recursion and pattern matching,
-- without using any standard library functions. For this reason,
-- you'll be working in a limited environment where almost none of the
-- standard library is available.
--
-- At least the following standard library functions are missing:
--  * (++)
--  * head
--  * tail
--  * map
--  * filter
--  * concat
--  * (!!)
--
-- The (:) operator is available, as is list literal syntax [a,b,c].
--
-- Feel free to use if-then-else, guards, and ordering functions (< and > etc.).
--
-- The tests will check that you haven't added imports :)

{-# LANGUAGE NoImplicitPrelude #-}

module Set3b where

import Mooc.LimitedPrelude
import Mooc.Todo

------------------------------------------------------------------------------
-- Ex 1: given numbers start, count and end, build a list that starts
-- with count copies of start and ends with end.
--
-- Use recursion and the : operator to build the list.
--
-- Examples:
--   buildList 1 5 2 ==> [1,1,1,1,1,2]
--   buildList 7 0 3 ==> [3]

buildList :: Int -> Int -> Int -> [Int]
buildList start count end = case count of 0 -> end : []
                                          _ -> start : (buildList start (count -1) end) 

------------------------------------------------------------------------------
-- Ex 2: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Use recursion and the : operator to build the list.
--
-- Ps. you'll probably need a recursive helper function
merge' :: [Int] -> [Int] -> [Int]
merge' [] xs = xs
merge' xs [] = xs
merge' (fs:xs) ys = fs : (merge' xs ys) 

sums :: Int -> [Int]
sums i = case i of 0 -> []
                   _ -> merge' (sums (i-1))  [( div (i*i + i)  2)] 


------------------------------------------------------------------------------
-- Ex 3: define a function mylast that returns the last value of the
-- given list. For an empty list, a provided default value is
-- returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
mylast def xs = case xs of [] -> def
                           (fx:[]) -> fx
                           (fx:xf) -> mylast def xf

------------------------------------------------------------------------------
-- Ex 4: safe list indexing. Define a function indexDefault so that
--   indexDefault xs i def
-- gets the element at index i in the list xs. If i is not a valid
-- index, def is returned.
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   indexDefault [True] 1 False         ==>  False
--   indexDefault [10,20,30] 0 7         ==>  10
--   indexDefault [10,20,30] 2 7         ==>  30
--   indexDefault [10,20,30] 3 7         ==>  7
--   indexDefault ["a","b","c"] (-1) "d" ==> "d"

indexDefault :: [a] -> Int -> a -> a
indexDefault [] _ def = def
indexDefault xs (-1) def = def
indexDefault (fs:xs) 0 def = fs
indexDefault (fs:xs) index def = indexDefault xs (index - 1) def

------------------------------------------------------------------------------
-- Ex 5: define a function that checks if the given list is in
-- increasing order.
--
-- Use pattern matching and recursion to iterate through the list.
--
-- Examples:
--   sorted [1,2,3] ==> True
--   sorted []      ==> True
--   sorted [2,7,7] ==> True
--   sorted [1,3,2] ==> False
--   sorted [7,2,7] ==> False

sorted :: [Int] -> Bool
sorted [] = True
sorted (fs : []) = True
sorted (fs : xs : ls) = if fs >  xs then False else sorted (xs: ls) 

------------------------------------------------------------------------------
-- Ex 6: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []
--
-- Use pattern matching and recursion (and the list constructors : and [])

sumsOf :: [Int] -> [Int]
sumsOf [] = []
sumsOf (fs:[]) = fs :[]
sumsOf (fs:xs:gs) = fs : (sumsOf (xs+fs:gs))

------------------------------------------------------------------------------
-- Ex 7: implement the function merge that merges two sorted lists of
-- Ints into a sorted list
--
-- Use only pattern matching and recursion (and the list constructors : and [])
--
-- Examples:
--   merge [1,3,5] [2,4,6] ==> [1,2,3,4,5,6]
--   merge [1,1,6] [1,2]   ==> [1,1,1,2,6]
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys                      -- if first list is empty, return second list
merge xs [] = xs                      -- if second list is empty, return first list
merge (x:xs) (y:ys)                   -- compare heads of both lists
  | x <= y    = x : merge xs (y:ys)   -- if x is smaller, take x and merge rest
  | otherwise = y : merge (x:xs) ys   -- if y is smaller, take y and merge rest

length' :: [Int] -> Int
length' [] = 0
length' (fx:xs) = 1 + (length' xs)
take' :: Int -> [Int] -> [Int]

take' cnt (fx:xs) = case cnt of 0 -> []
                                _ -> fx : take' (cnt - 1) xs 
drop' :: Int -> [Int] -> [Int]

drop' cnt (fx:xs) = drophelper' ((length' (fx:xs)) - cnt ) (fx:xs)
                    where drophelper' _ [] = []
                          drophelper' 0 (fx:xs) = (fx:xs)
                          drophelper' cnt (fx:xs) = drophelper' (cnt-1) xs

even' = (\x -> (mod x 2 == 0))

mergeSort :: [Int] -> [Int]
-- split
mergeSort [] = []
mergeSort (fx:[]) = fx:[]
mergeSort (fx:xs:[]) = (if fx < xs then fx:xs:[] else xs:fx:[])
mergeSort (fx:xs:xm) = merge ( mergeSort ( take' (if (even' n) then (div n 2)
                                                              else (div n 2 + 1))
                                                (fx:xs:xm)) )
                             ((mergeSort ( drop' ((div n 2))(fx:xs:xm)) ))
                       where n = length' (fx:xs:xm)
------------------------------------------------------------------------------
-- Ex 8: compute the biggest element, using a comparison function
-- passed as an argument.
--
-- That is, implement the function mymaximum that takes
--
-- * a function `bigger` :: a -> a -> Bool
-- * a value `initial` of type a
-- * a list `xs` of values of type a
--
-- and returns the biggest value it sees, considering both `initial`
-- and all element in `xs`.
--
-- Examples:
--   mymaximum (>) 3 [] ==> 3
--   mymaximum (>) 0 [1,3,2] ==> 3
--   mymaximum (>) 4 [1,3,2] ==> 4    -- initial value was biggest
--   mymaximum (<) 4 [1,3,2] ==> 1    -- note changed biggerThan
--   mymaximum (\(a,b) (c,d) -> b > d) ("",0) [("Banana",7),("Mouse",8)]
--     ==> ("Mouse",8)

mymaximum :: (a -> a -> Bool) -> a -> [a] -> a

mymaximum bigger initial [] = initial
mymaximum bigger initial (fs:xs) = if (bigger fs initial) then (mymaximum bigger fs xs) else (mymaximum bigger initial xs)

------------------------------------------------------------------------------
-- Ex 9: define a version of map that takes a two-argument function
-- and two lists. Example:
--
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- If the lists have differing lengths, ignore the trailing elements
-- of the longer list.
--
-- Use recursion and pattern matching. Do not use any library functions.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] [] = []
map2 f [] ys = []
map2 f xs [] = []
map2 f (fx:xs) (fy:ys) = (f fx fy) : (map2 f xs ys)
-- map2 f as bs = todo

------------------------------------------------------------------------------
-- Ex 10: implement the function maybeMap, which works a bit like a
-- combined map & filter.
---
-- maybeMap is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, no value gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in maybeMap f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- maybeMap Just [1,2,3]
--   ==> [1,2,3]
--
-- maybeMap (\x -> Nothing) [1,2,3]
--   ==> []

maybeMap :: (a -> Maybe b) -> [a] -> [b]
maybeMap _ [] = []
maybeMap f (fx:xs) = case (f fx) of Nothing -> maybeMap f xs
                                    Just x -> x : maybeMap f xs
