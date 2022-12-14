package constraints

sealed trait not[A]

infix sealed trait and[A, B]
infix sealed trait or[A, B]
infix sealed trait xor[A, B]

infix type implies[A, B] = not[A] or B
infix type nand[A, B] = not[A and B]
infix type nor[A, B] = not[A or B]
infix type xnor[A, B] = not[A xor B]

type ForAll[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], true, and]
type Exists[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], false, or]

type Inverse[P[_]] = [X] =>> not[P[X]]
