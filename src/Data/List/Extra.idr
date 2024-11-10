module Data.List.Extra

import Data.List

import Prop.List

infixl 9 !!
public export
(!!) : (l : List a) -> (n : Nat) -> {auto 0 prf : Has (S n) l} -> a
(!!) (x :: _) Z = x
(!!) (_ :: xs) (S n) {prf = HasS _ _ prfPred} = xs !! n

infixl 9 !!?
public export
(!!?) : List a -> Nat -> Maybe a
Nil       !!? n = Nothing
(x :: _)  !!? Z = Just x
(_ :: xs) !!? S n = xs !!? n

public export
ofLength : Nat -> List a -> Bool
ofLength n l = length l == n

||| Returns all lists that can be made by concatenating a list from the first
||| set of list with a list from the second.
public export
allConcats : List (List a) -> List (List a) -> List (List a)
allConcats l1s l2s = do
  l1 <- l1s
  l2 <- l2s

  pure (l1 ++ l2)

||| sort in descending order according to an ordering
public export
sortDescBy : (a -> a -> Ordering) -> List a -> List a
sortDescBy = sortBy . flip

||| sort a list in descending order
public export
sortDesc : Ord a => List a -> List a
sortDesc = sortDescBy compare

public export
subsequences : List a -> List (List a)
subsequences xs = Nil :: nonEmptySubsequences xs where

  nonEmptySubsequences : List a -> List (List a)
  nonEmptySubsequences Nil = Nil
  nonEmptySubsequences (x :: xs) = [x] :: foldr f [] (nonEmptySubsequences xs)
    where
      f : List a -> List (List a) -> List (List a)
      f ys r = ys :: (x :: ys) :: r

public export
headIfNonEmpty : (l : List a) -> (NonEmpty l) -> a
headIfNonEmpty l prf = head l

||| Delete all occurances of an element from a list
public export
deleteAll : Eq a => a -> List a -> List a
deleteAll _ Nil = Nil
deleteAll x (x' :: xs) = if x == x' then deleteAll x xs else x' :: deleteAll x xs
