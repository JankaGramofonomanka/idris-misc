module Data.List1.Extra

import Data.List1

public export
maximum : Ord a => List1 a -> a
maximum (x ::: xs) = maximum' x xs where
  maximum' : a -> List a -> a
  maximum' x Nil = x
  maximum' x1 (x2 :: xs) = if x1 > x2 then maximum' x1 xs else maximum' x2 xs

public export
partial
listToList1 : List a -> List1 a
listToList1 (x :: xs) = x ::: xs

public export
list1ToList : List1 a -> List a
list1ToList (x ::: xs) = x :: xs

export infixr 7 +++
||| Concatenate a non-empty list with a list
public export
(+++) : List1 a -> List a -> List1 a
(x ::: xs) +++ ys = x ::: xs ++ ys
