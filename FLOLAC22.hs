module FLOLAC22 where

import Data.List

-- This exercise covers the first 6 and the 8th chapters of "Learn You a Haskell for Great Good!"

-- Chapter 1 - http://learnyouahaskell.com/introduction
-- Chapter 2 - http://learnyouahaskell.com/starting-out
-- Chapter 3 - http://learnyouahaskell.com/types-and-typeclasses
-- Chapter 4 - http://learnyouahaskell.com/syntax-in-functions
-- Chapter 5 - http://learnyouahaskell.com/recursion
-- Chapter 6 - http://learnyouahaskell.com/higher-order-functions
-- Chapter 8 - http://learnyouahaskell.com/making-our-own-types-and-typeclasses 

-- Download this file and then type ":l FLOLAC-22-Tasks.hs" in GHCi to load this exercise
-- Some of the definitions are left "undefined", you should replace them with your answers.

-- 0. Example: find the penultimate (second-to-last) element in list xs
penultimate :: [a] -> a
penultimate xs = last (init xs)

-- 1. Find the antepenultimate (third-to-last) element in list xs
antepenultimate :: [a] -> a
antepenultimate xs = penultimate (init xs)

-- 2. Left shift list xs by 1
--    For example, "shiftLeft [1, 2, 3]" should return "[2, 3, 1]"
shiftLeft :: [a] -> [a]
shiftLeft [] = []
shiftLeft (x:xs) = xs ++ [x]

-- 3. Left shift list xs by n
--    For example, "rotateLeft 2 [1, 2, 3]" should return "[3, 1, 2]"
rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs
  | n < 0 = error "Negative shift value"
  | n == 0 = xs
  | n > 0 = rotateLeft (n-1) (shiftLeft xs)

-- 4. Insert element x in list xs at index k
--    For example, "insertElem 100 3 [0,0,0,0,0]" should return [0,0,0,100,0,0]
insertElem :: a -> Int -> [a] -> [a]
insertElem x k xs
  | k < 0 = error "Negative insert index"
  | otherwise = (take k xs) ++ [x] ++ (drop k xs)

-- Here we have a type for the 7 days of the week
-- Try typeclass functions like "show" or "maxBound" on them
data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
         deriving (Eq, Ord, Show, Bounded, Enum)

-- 5. Note that if you try "succ Sun", you should get an error, because "succ" is not defined on "Sun"
--    Define "next", which is like "succ", but returns "Mon" on "next Sun"
next :: Day -> Day
next day
  | day == Sun = Mon
  | otherwise = succ day

-- 6. Return "True" on weekend
isWeekend :: Day -> Bool
isWeekend day = day >= Sat

data Task = Work | Shop | Play deriving (Eq, Show)

-- You are given a schedule, which is a list of pairs of Tasks and Days
schedule :: [(Task, Day)]
schedule = [(Shop, Fri), (Work, Tue), (Play, Mon), (Play, Fri)]

-- 7. However, the schedule is a mess
--    Sort the schedule by Day, and return only a list of Tasks.
--    If there are many Tasks in a Day, you should keep its original ordering
--    For example, "sortTask schedule" should return "[(Play, Mon), (Work, Tue), (Shop, Fri), (Play, Fri)]"
sortTask :: [(Task, Day)] -> [(Task, Day)]
sortTask = sortBy (\ta tb -> (snd ta) `compare` (snd tb))

-- 8. This function converts days to names, like "show", but a bit fancier
--    For example, "nameOfDay Mon" should return "Monday"
nameOfDay :: Day -> String
nameOfDay Mon = "Monday"
nameOfDay Tue = "Tuesday"
nameOfDay Wed = "Wednesday"
nameOfDay Thu = "Thursday"
nameOfDay Fri = "Friday"
nameOfDay Sat = "Saturday"
nameOfDay Sun = "Sunday"

-- 9. You shouldn't be working on the weekends
--     Return "False" if the Task is "Work" and the Day is "Sat" or "Sun"
labourCheck :: Task -> Day -> Bool
labourCheck task day = not $ task == Work && isWeekend day

-- 10. Raise x to the power y using recursion
--     For example, "power 3 4" should return "81"
power :: Int -> Int -> Int
power x y
  | y < 0 = error "negative power"
  | y == 0 = 1
  | y > 0 = x * power x (y-1)

-- 11. Convert a list of booleans (big-endian) to a interger using recursion
--     For example, "convertBinaryDigit [True, False, False]" should return 4
convertBinaryDigit :: [Bool] -> Int
convertBinaryDigit bits = convert bits 0
  where
    convert :: [Bool] -> Int -> Int
    convert [] val = val
    convert (x:xs) val
      | x = convert xs (val * 2 + 1)
      | otherwise = convert xs (val * 2)

-- 12. Create a fibbonaci sequence of length N in reverse order
--     For example, "fib 5" should return "[3, 2, 1, 1, 0]"
fib :: Int -> [Int]
fib 0 = []
fib 1 = [0]
fib 2 = [1,0]
fib n = let l = fib (n-1)
  in (head l + head (tail l)) : l

-- 13. Determine whether a given list is a palindrome
--     For example, "palindrome []" or "palindrome [1, 3, 1]" should return "True"
palindrome :: Eq a => [a] -> Bool
palindrome xs = let revxs = reverse xs
  in xs == revxs

-- 14. Map the first component of a pair with the given function
--     For example, "mapFirst (+3) (4, True)" should return "(7, True)"
mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f pair = (f (fst pair), snd pair)

-- 15. Devise a function that has the following type
someFunction :: (a -> b -> c) -> (a -> b) -> a -> c
someFunction f1 f2 a = (f1 a) (f2 a)

-- Here is an algebraic datatype representing trees.
-- Be careful, these trees are somehow different from those defined in the book!
-- Apparently our trees are better, they have leaves, and data is stored on leaves.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- 16. What is the map function as well as the Functor instance for this tree?
--     (You may want to lookup the Functor typeclass.)
instance Functor Tree where
  fmap = undefined

-- We say a tree is flattenable if it can be turned into a list
-- which contains all elements originally in the tree.
data List a = Nil | Cons a (List a)

-- We can define a typeclass to express that.
class Flattenable t where
  flatten :: t a -> List a

-- 17. Show that our Tree is flattenable:
instance Flattenable Tree where
  flatten = undefined

-- 18. Define a type of trees that have leaves and two kinds of nodes:
--     one with two branches and another with three branches.
--     Your tree should have three constructors.
--     You can choose where to store data (either on leaves, nodes, or both).
--
--   data TwoThreeTree = ...

-- 19. Show that the datatype you just defined is flattenable:
--
--   instance Flattenable TwoThreeTree where
--     flatten = ...