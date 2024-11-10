module Data.Vect.Extra

import Data.List
import Data.Vect

public export
allConcats : List (Vect n a) -> List (Vect m a) -> List (Vect (n + m) a)
allConcats l1s l2s = do
  l1 <- l1s
  l2 <- l2s

  pure (l1 ++ l2)

-- TODO: get rid of `believe_me`
public export
sortBy : (a -> a -> Ordering) -> Vect n a -> Vect n a
sortBy cmp v = believe_me $ Data.Vect.fromList $ Data.List.sortBy cmp (toList v)

public export
sortVect : Ord a => Vect n a -> Vect n a
sortVect = sortBy compare

public export
sortDescBy : (a -> a -> Ordering) -> Vect n a -> Vect n a
sortDescBy = sortBy . flip

public export
sortDesc : Ord a => Vect n a -> Vect n a
sortDesc = sortDescBy compare

public export
vectToList : Vect n a -> List a
vectToList = toList

public export
subsequencesOfLength : (n : Nat) -> List a -> List (Vect n a)
subsequencesOfLength Z _ = [Nil]
subsequencesOfLength (S k) Nil = Nil
subsequencesOfLength (S k) (x :: xs) = (map (x ::) (sol k xs)) ++ sol (S k) xs
  where
    sol : (p : Nat) -> List a -> List (Vect p a)
    sol = subsequencesOfLength

public export
subsequencesOfLengthVect : (n : Nat) -> Vect m a -> List (Vect n a)
subsequencesOfLengthVect Z _ = [Nil]
subsequencesOfLengthVect (S k) Nil = Nil
subsequencesOfLengthVect (S k) (x :: xs) = (map (x ::) (sol k xs)) ++ sol (S k) xs
  where
    sol : (p : Nat) -> Vect q a -> List (Vect p a)
    sol = subsequencesOfLengthVect

public export
filterLength : (n : Nat) -> List (List a) -> List (Vect n a)
filterLength n l = foldl (appendIfLengthIs n) [] l where
  appendIfLengthIs : (k : Nat) -> List (Vect k a) -> List a -> List (Vect k a)
  appendIfLengthIs k acc elem = case toVect k elem of
    Nothing => acc
    Just v  => acc ++ [v]
