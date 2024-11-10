||| Properties of lists, in terms of `Eq` implementation
module Prop.List.Eq

import Data.So
import public Prop.List

%hide Prop.List.Elem
%hide Prop.List.NotElem
%hide Prop.List.Precedes
%hide Prop.List.Succeeds

public export
data Elem : Eq a => a -> List a -> Type where
  ElemHead : Eq a => {x, y : a} -> So (x == x') -> x `Elem` (x' :: xs)
  ElemTail : Eq a => {x, y : a} -> x `Elem` xs  -> x `Elem` (x' :: xs)

public export
NotElem : Eq a => a -> List a -> Type
NotElem x l = Forall l (\y => So (not $ x == y))

-- TODO make `Precedes` associative
public export
data Precedes : Eq a => a -> a -> List a -> Type where
  PrecedesHd  : Eq a => (e1 : a) -> (e2 : a) -> e2 `Elem` t               -> (e1 `Precedes` e2) `In` (e1 :: t)
  PrecedesInd : Eq a => (e1 : a) -> (e2 : a) -> (e1 `Precedes` e2) `In` t -> (e1 `Precedes` e2) `In` (h :: t)

public export
Succeeds : Eq a => a -> a -> List a -> Type
Succeeds = flip Precedes
