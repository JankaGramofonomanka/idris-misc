module Data.Stream.Extra

infixl 9 !!
public export
(!!) : Stream a -> Nat -> a
(x :: _) !! Z = x
(_ :: xs) !! S n = xs !! n

-- TODO this will loop, because idris is not lazy, do sth about it
-- TODO or maybe not, what does `Inf` do?
public export
prepend : List a -> Stream a -> Stream a
prepend Nil s = s
prepend (x :: xs) s = x :: prepend xs s

