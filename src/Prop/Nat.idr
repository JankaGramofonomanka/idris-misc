module Prop.Nat

public export
data Before : Nat -> Nat -> Type where
  BeforeBase : {n : Nat} -> Before n (S n)
  BeforeInd : {n, m : Nat} -> Before n m -> Before n (S m)
