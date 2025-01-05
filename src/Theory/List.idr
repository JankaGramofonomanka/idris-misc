module Theory.List

import Data.List

import Theory.General

||| Concatenation of lists os associative
total
export
concat_assoc : (l, l', l'' : List a) -> l ++ (l' ++ l'') = (l ++ l') ++ l''
concat_assoc Nil l' l'' = Refl
concat_assoc (x :: xs) l' l'' = rewrite revEq $ concat_assoc {l = xs, l', l''} in Refl

||| Concatenating a list with an emoty list results in the same list
total
export
concat_nil : (l : List a) -> l ++ Nil = l
concat_nil Nil = Refl
concat_nil (x :: xs) = rewrite concat_nil xs in Refl


||| The mapping of a function on a concatenation of two lists
||| is the concatenation of mappings of that function on those lists
total
export
map_concat
   : {f : a -> b}
  -> (l, l' : List a)
  -> map f (l ++ l') = map f l ++ map f l'
map_concat {f} Nil l = Refl
map_concat {f} (x :: xs) l = rewrite revEq $ map_concat {f} xs l in Refl


||| `map_concat` in the case when the right operand is a singleton
total
export
map_append
   : {f : a -> b}
  -> (l : List a)
  -> (x : a)
  -> map f (l ++ [x]) = map f l ++ [f x]
map_append l x = map_concat l [x]

||| The concatenation of a non-empty list with another list is non-empty
total
export
nonempty_concat_left' : (xs, ys : List a) -> NonEmpty xs -> NonEmpty (xs ++ ys)
nonempty_concat_left' Nil        ys        IsNonEmpty impossible
nonempty_concat_left' (x :: xs)  Nil       IsNonEmpty = IsNonEmpty
nonempty_concat_left' (x :: xs)  (y :: ys) IsNonEmpty = IsNonEmpty

||| The concatenation of a non-empty list with another list is non-empty
total
export
nonempty_concat_left : {xs, ys : List a} -> NonEmpty xs -> NonEmpty (xs ++ ys)
nonempty_concat_left {xs, ys} = nonempty_concat_left' xs ys

||| The concatenation of a list with a non-empty list is non-empty
total
export
nonempty_concat_right' : (xs, ys : List a) -> NonEmpty ys -> NonEmpty (xs ++ ys)
nonempty_concat_right' xs         Nil       IsNonEmpty impossible
nonempty_concat_right' Nil        (y :: ys) IsNonEmpty = IsNonEmpty
nonempty_concat_right' (x :: xs)  (y :: ys) IsNonEmpty = IsNonEmpty

||| The concatenation of a list with a non-empty list is non-empty
total
export
nonempty_concat_right : {xs, ys : List a} -> NonEmpty ys -> NonEmpty (xs ++ ys)
nonempty_concat_right {xs, ys} = nonempty_concat_right' xs ys

||| If   the concatenation of one list  with another is non-empty,
||| then the concatenation of the other with the one is non-empty
total
export
nonempty_flip_concat' : (xs, ys : List a) -> NonEmpty (xs ++ ys) -> NonEmpty (ys ++ xs)
nonempty_flip_concat' Nil        Nil       IsNonEmpty impossible
nonempty_flip_concat' Nil        (y :: ys) IsNonEmpty = IsNonEmpty
nonempty_flip_concat' (x :: xs)  Nil       IsNonEmpty = IsNonEmpty
nonempty_flip_concat' (x :: xs)  (y :: ys) IsNonEmpty = IsNonEmpty

||| If   the concatenation of one list  with another is non-empty,
||| then the concatenation of the other with the one is non-empty
total
export
nonempty_flip_concat : {xs, ys : List a} -> NonEmpty (xs ++ ys) -> NonEmpty (ys ++ xs)
nonempty_flip_concat {xs, ys} = nonempty_flip_concat' xs ys

||| If a list is non-empty, then any mapping on that list is also non-empty
total
export
nonempty_map' : (xs : List a) -> (f : a -> b) -> NonEmpty xs -> NonEmpty (map f xs)
nonempty_map' Nil       f IsNonEmpty impossible
nonempty_map' (x :: xs) f IsNonEmpty = IsNonEmpty

||| If a list is non-empty, then any mapping on that list is also non-empty
total
export
nonempty_map : {xs : List a} -> {f : a -> b} -> NonEmpty xs -> NonEmpty (map f xs)
nonempty_map {xs} {f} = nonempty_map' xs f

||| If the concatenation of two lists is non-empty, then one of the two lists is non-empty
total
export
nonempty_sublist' : (xs, ys : List a) -> NonEmpty (xs ++ ys) -> Either (NonEmpty xs) (NonEmpty ys)
nonempty_sublist' Nil        Nil       IsNonEmpty impossible
nonempty_sublist' Nil        (y :: ys) IsNonEmpty = Right IsNonEmpty
nonempty_sublist' (x :: xs)  ys        IsNonEmpty = Left IsNonEmpty

||| If the concatenation of two lists is non-empty,
||| then one of the two lists is non-empty
total
export
nonempty_sublist : {xs, ys : List a} -> NonEmpty (xs ++ ys) -> Either (NonEmpty xs) (NonEmpty ys)
nonempty_sublist {xs, ys} = nonempty_sublist' xs ys

||| If   the concatenation of two               lists is non-empty,
||| then the concatenation of mappings on those lists is non-empty
total
export
nonempty_concat_map'
   : (xs, ys : List a)
  -> (f, g : a -> b)
  -> NonEmpty (xs ++ ys)
  -> NonEmpty (map f xs ++ map g ys)
nonempty_concat_map' Nil       Nil       f g IsNonEmpty impossible
nonempty_concat_map' Nil       (y :: ys) f g IsNonEmpty = IsNonEmpty
nonempty_concat_map' (x :: xs) ys        f g IsNonEmpty = IsNonEmpty

||| If   the concatenation of two               lists is non-empty,
||| then the concatenation of mappings on those lists is non-empty
total
export
nonempty_concat_map
   : {xs, ys : List a}
  -> {f, g : a -> b}
  -> NonEmpty (xs ++ ys)
  -> NonEmpty (map f xs ++ map g ys)
nonempty_concat_map {xs, ys, f, g} = nonempty_concat_map' xs ys f g
