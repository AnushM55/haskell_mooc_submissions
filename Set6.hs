-- Exercise set 6: defining classes and instances

module Set6 where

import Mooc.Todo
import Data.Char (toLower)

------------------------------------------------------------------------------
-- Ex 1: define an Eq instance for the type Country below. You'll need
-- to use pattern matching.

data Country = Finland | Switzerland | Norway
  deriving Show

instance Eq Country where
  Finland == Finland = True
  Switzerland == Switzerland = True
  Norway == Norway = True
  _ == _ = False

------------------------------------------------------------------------------
-- Ex 2: implement an Ord instance for Country so that
--   Finland <= Norway <= Switzerland
--
-- Remember minimal complete definitions!

instance Ord Country where
  compare x y | x == y = EQ
              | x /= y = case (x,y) of (Finland,Switzerland) -> LT
                                       (Switzerland,Finland) -> GT
                                       (Finland,Norway) -> LT
                                       (Norway,Finland) -> GT
                                       (Norway,Switzerland) -> LT
                                       (Switzerland, Norway) -> GT
  (<=) x y = compare x y /= GT
  min x y | x <= y = x
          | otherwise = y
  max x y | x <= y = y
          | otherwise = x

------------------------------------------------------------------------------
-- Ex 3: Implement an Eq instance for the type Name which contains a String.
-- The Eq instance should ignore capitalization.
--
-- Hint: use the function Data.Char.toLower that has been imported for you.
--
-- Examples:
--   Name "Pekka" == Name "pekka"   ==> True
--   Name "Pekka!" == Name "pekka"  ==> False

data Name = Name String
  deriving Show

instance Eq Name where
  (==) (Name "") (Name "") = True
  (==) (Name "") (Name fs) = False
  (==) (Name fs) (Name "")  = False
  (==) (Name (fs1:xs)) (Name (fs2:ys)) = case ((toLower fs1) == (toLower fs2)) of False -> False
                                                                                  True -> ((==) (Name xs) (Name ys))

------------------------------------------------------------------------------
-- Ex 4: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq (List a)" that compares the lists element
-- by element.
--
-- Note how the instance needs an Eq a constraint. What happens if you
-- remove it?

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
  (==) Empty Empty = True
  (==) Empty node = False
  (==) node Empty = False
  (==) (LNode val1 rest1) (LNode val2 rest2) = case (val1 == val2) of False -> False
                                                                      True -> (==) rest1 rest2

------------------------------------------------------------------------------
-- Ex 5: below you'll find two datatypes, Egg and Milk. Implement a
-- type class Price, containing a function price. The price function
-- should return the price of an item.
--
-- The prices should be as follows:
-- * chicken eggs cost 20
-- * chocolate eggs cost 30
-- * milk costs 15 per liter
--
-- Example:
--   price ChickenEgg  ==>  20

data Egg = ChickenEgg | ChocolateEgg
  deriving Show
data Milk = Milk Int -- amount in litres
  deriving Show

class Price a where
    price :: a -> Int

instance Price Egg where
    price ChickenEgg = 20
    price ChocolateEgg = 30

instance Price Milk where
    price (Milk litre) = litre*15
------------------------------------------------------------------------------
-- Ex 6: define the necessary instance hierarchy in order to be able
-- to compute these:
--
-- price (Just ChickenEgg) ==> 20
-- price [Milk 1, Milk 2]  ==> 45
-- price [Just ChocolateEgg, Nothing, Just ChickenEgg]  ==> 50
-- price [Nothing, Nothing, Just (Milk 1), Just (Milk 2)]  ==> 45

instance Price a => Price (Maybe a) where
    price (Just something) = price something
    price Nothing = 0

instance Price a => Price [a] where
    price [] = 0
    price (fs:xs) = (price fs ) + (price xs) 

------------------------------------------------------------------------------
-- Ex 7: below you'll find the datatype Number, which is either an
-- Integer, or a special value Infinite.
--
-- Implement an Ord instance so that finite Numbers compare normally,
-- and Infinite is greater than any other value.

data Number = Finite Integer | Infinite
  deriving (Show,Eq)

instance  Ord Number where
        
        (<=) (Finite num1) (Finite num2) | num1 < num2 = True
                                         | num1 > num2 = False
                                         | num1 == num2 = True
        (<=) Infinite (Finite _) = False
        (<=) _ Infinite = True
    

------------------------------------------------------------------------------
-- Ex 8: rational numbers have a numerator and a denominator that are
-- integers, usually separated by a horizontal bar or a slash:
--
--      numerator
--    -------------  ==  numerator / denominator
--     denominator
--
-- You may remember from school that two rationals a/b and c/d are
-- equal when a*d == b*c. Implement the Eq instance for rationals
-- using this definition.
--
-- You may assume in all exercises that the denominator is always
-- positive and nonzero.
--
-- Examples:
--   RationalNumber 4 5 == RationalNumber 4 5    ==> True
--   RationalNumber 12 15 == RationalNumber 4 5  ==> True
--   RationalNumber 13 15 == RationalNumber 4 5  ==> False

data RationalNumber = RationalNumber Integer Integer
  deriving Show

instance Eq RationalNumber where
  (RationalNumber a b) == (RationalNumber c d) = ((a*d) == (b*c)) 

------------------------------------------------------------------------------
-- Ex 9: implement the function simplify, which simplifies a rational
-- number by removing common factors of the numerator and denominator.
-- In other words,
--
--     ca         a
--    ----  ==>  ---
--     cb         b
--
-- As a concrete example,
--
--     12        3 * 4         4
--    ----  ==  -------  ==>  ---.
--     15        3 * 5         5
--
-- Hint: Remember the function gcd?

simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber a b) = case (gcd a b) of 1 -> (RationalNumber a b)
                                                  val -> (RationalNumber (div a val) (div b val))

------------------------------------------------------------------------------
-- Ex 10: implement the typeclass Num for RationalNumber. The results
-- of addition and multiplication must be simplified.
--
-- Reminders:
--   * negate x is 0-x
--   * abs is absolute value
--   * signum is -1, +1 or 0 depending on the sign of the input
--
-- Examples:
--   RationalNumber 1 3 + RationalNumber 1 6 ==> RationalNumber 1 2
--   RationalNumber 1 3 * RationalNumber 3 1 ==> RationalNumber 1 1
--   negate (RationalNumber 2 3)             ==> RationalNumber (-2) 3
--   fromInteger 17 :: RationalNumber        ==> RationalNumber 17 1
--   abs (RationalNumber (-3) 2)             ==> RationalNumber 3 2
--   signum (RationalNumber (-3) 2)          ==> RationalNumber (-1) 1
--   signum (RationalNumber 0 2)             ==> RationalNumber 0 1

instance Num RationalNumber where
  (RationalNumber a b) + (RationalNumber c d) = simplify ( RationalNumber ((a*d) + (b*c)) (b*d)) 
  (RationalNumber a b) * (RationalNumber c d) = simplify (RationalNumber (a*c) (b*d))
  abs (RationalNumber a b) = (RationalNumber (if a < 0 then -a else a) (if b < 0 then -b else b))
  signum (RationalNumber 0 0) = RationalNumber 0 0
  signum (RationalNumber a 0) = RationalNumber a 0
  signum (RationalNumber 0 a) = RationalNumber 0 a
  signum (RationalNumber a b) = RationalNumber (if a > 0 then 1 else -1) ( if b > 0 then 1 else -1)
  fromInteger x = (RationalNumber x 1)
  negate (RationalNumber a b)= RationalNumber (-a) b

------------------------------------------------------------------------------
-- Ex 11: a class for adding things. Define a class Addable with a
-- constant `zero` and a function `add`. Define instances of Addable
-- for Integers and lists. Numbers are added with the usual addition,
-- while lists are added by catenating them. Pick a value for `zero`
-- such that: `add zero x == x`
--
-- Examples:
--   add 1 2                ==>  3
--   add 1 zero             ==>  1
--   add [1,2] [3,4]        ==>  [1,2,3,4]
--   add zero [True,False]  ==>  [True,False]
class (Show a, Eq a) => Addable a where
    add  :: a -> a -> a 
    zero :: a

instance  Addable Integer where
    zero = 0
    add x y = x+y

instance Addable Bool where
    zero = True

    add zero x = zero

instance (Show a,Eq a , Addable a) => Addable [a] where
    zero = []

    add (fs : []) (gs : []) =  fs:gs:[]
    add (fs:xs) (gs:ys)= fs:(add xs (gs:ys))
    add [] (fs:xs)= (fs:xs)
    add (fs:xs) [] = (fs:xs)
    add [] [] = zero
------------------------------------------------------------------------------
-- Ex 12: cycling. Implement a type class Cycle that contains a
-- function `step` that cycles through the values of the type.
-- Implement instances for Color and Suit that work like this:
--
--   step Red ==> Green
--   step Green ==> Blue
--   step Blue ==> Red
--
-- The suit instance should cycle suits in the order Club, Spade,
-- Diamond, Heart, Club.
--
-- Also add a function `stepMany` to the class and give it a default
-- implementation using `step`. The function `stepMany` should take
-- multiple (determined by an Int argument) steps like this:
--
--   stepMany 2 Club ==> Diamond
--   stepMany 3 Diamond ==> Spade
--
-- The tests will test the Cycle class and your default implementation
-- of stepMany by adding an instance like this:
--
--    instance Cycle Int where
--      step = succ

data Color = Red | Green | Blue
  deriving (Show, Eq)
data Suit = Club | Spade | Diamond | Heart
  deriving (Show, Eq)

class (Show a) =>  Cycle a where
    step :: a -> a

    stepMany :: Int -> a -> a


    stepMany 0 someState = someState
    stepMany val someState = stepMany (val-1) (step someState)


instance Cycle Suit where
    step suitname = case suitname of Club -> Spade
                                     Spade -> Diamond
                                     Diamond -> Heart
                                     Heart -> Club
    
instance Cycle Color where
    step color = case color of Red -> Green
                               Green -> Blue
                               Blue -> Red
