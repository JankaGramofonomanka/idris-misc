||| A module with general theoremss
module Theory.General

||| Equality is symmetric
total
export
revEq : a = b -> b = a
revEq Refl = Refl

||| From falsehood, anything follows
||| a.k.a the principle of explosion
total
export
exfalso : Void -> a
exfalso v = case v of {}

