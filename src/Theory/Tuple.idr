
module Theory.Tuple

||| A tuple consists of its first and second element
total
export
tuple_destruct : (t : (a, b)) -> t = (fst t, snd t)
tuple_destruct (x, y) = Refl
