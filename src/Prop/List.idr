||| Properties of lists
module Prop.List

public export
data Forall : List a -> (a -> Type) -> Type where
  ForallNil : Forall Nil prop
  ForallCons : {x : a} -> prop x -> Forall xs prop -> Forall (x :: xs) prop

public export
data Elem : a -> List a -> Type where
  ElemHead : x `Elem` (x :: xs)
  ElemTail : x `Elem` xs -> x `Elem` (x' :: xs)

public export
NotElem : a -> List a -> Type
NotElem x l = Forall l (\y => Not (x = y))

public export
In : (List a -> Type) -> List a -> Type
In = ($)

-- TODO make `Precedes` associative
public export
data Precedes : a -> a -> List a -> Type where
  PrecedesHd
     : (e1 : a) -> (e2 : a)
    -> e2 `Elem` t
    -> (e1 `Precedes` e2) `In` (e1 :: t)

  PrecedesInd
     : (e1 : a) -> (e2 : a)
    -> (e1 `Precedes` e2) `In` t
    -> (e1 `Precedes` e2) `In` (h :: t)

public export
Succeeds : a -> a -> List a -> Type
Succeeds = flip Precedes

public export
data Has : Nat -> List a -> Type where
  HasZ : (l : List a) -> Has 0 l
  HasS : (x : a) -> (xs : List a) -> Has n xs -> Has (S n) (x :: xs)
