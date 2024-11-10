module Theory.List1

import Data.List1

import Theory.List

||| Concatenation of non-empty lists os associative
total
export
concat_assoc : (l, l', l'' : List1 a) -> l ++ (l' ++ l'') = (l ++ l') ++ l''
concat_assoc (x ::: xs) (y ::: ys) (z ::: zs)
  = rewrite List.concat_assoc xs (y :: ys) (z :: zs)
    in Refl

||| The mapping of a function on a concatenation of two non-empty lists
||| is the concatenation of mappings of that function on those lists
total
export
map_concat : {f : a -> b}
          -> (l, l' : List1 a)
          -> map f (l ++ l') = map f l ++ map f l'
map_concat {f} (x ::: xs) (y ::: ys)
  = rewrite List.map_concat {f} xs (y :: ys)
    in Refl


||| `map_concat` in the case when the right operand is a singleton
total
export
map_append : {f : a -> b}
          -> (l : List1 a)
          -> (x : a)
          -> map f (l ++ (x ::: Nil)) = map f l ++ (f x ::: Nil)
map_append l x = map_concat l (x ::: Nil)



