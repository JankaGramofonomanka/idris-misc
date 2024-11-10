module Utils

namespace Nat

  public export
  data Before : Nat -> Nat -> Type where
    BeforeBase : {n : Nat} -> Before n (S n)
    BeforeInd : {n, m : Nat} -> Before n m -> Before n (S m)


namespace Stream

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



namespace List

  public export
  data Has : Nat -> List a -> Type where
    HasZ : (l : List a) -> Has 0 l
    HasS : (h : a) -> (t : List a) -> Has n t -> Has (S n) (h :: t)
  
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
  data Elem : a -> List a -> Type where
    InHead : (h : a) -> h `Elem` h :: t
    InTail : (h : a) -> e `Elem` t -> e `Elem` (h :: t)

  public export
  In : (List a -> Type) -> List a -> Type
  In = ($)

  -- TODO make some parameters implicit
  -- TODO make `Precedes` associative
  public export
  data Precedes : a -> a -> List a -> Type where
    PrecedesHd  : (e1 : a) -> (e2 : a) -> e2 `Elem` t               -> (e1 `Precedes` e2) `In` (e1 :: t)
    PrecedesInd : (e1 : a) -> (e2 : a) -> (e1 `Precedes` e2) `In` t -> (e1 `Precedes` e2) `In` (h :: t)

  public export
  Succeeds : a -> a -> List a -> Type
  Succeeds = flip Precedes

