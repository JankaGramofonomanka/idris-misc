module Extra

||| Apply a function to the first element of a tuple
export
onFirst : (a -> b) -> (a, c) -> (b, c)
onFirst f (x, y) = (f x, y)

||| Apply a function to the second element of a tuple
export
onSecond : (b -> c) -> (a, b) -> (a, c)
onSecond f (x, y) = (x, f y)

||| Convert a function on a triples to a three-parameter function
export
curry3 : ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

||| Convert a three-parameter function to a function on triples
export
uncurry3 : (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

||| Right fold on a list of triples, using a a three-parameter function
export
foldr3 : Foldable t => (e1 -> e2 -> e3 -> acc -> acc) -> acc -> t (e1, e2, e3) -> acc
foldr3 = foldr . uncurry3

||| Left fold on a list of triples, using a a three-parameter function
export
foldl3 : Foldable t => (acc -> e1 -> e2 -> e3 -> acc) -> acc -> t (e1, e2, e3) -> acc
foldl3 f = foldl (uncurry3 . f)

