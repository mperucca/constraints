package constraints

infix sealed trait And[A, B]
infix sealed trait Not[A]
infix sealed trait Or[A, B]
infix sealed trait Xor[A, B]
infix sealed trait True
infix sealed trait False

infix type Implies[A, B] = Not[A] Or B
infix type Nand[A, B] = Not[A And B]
infix type Nor[A, B] = Not[A Or B]
infix type Xnor[A, B] = Not[A Xor B]
type ForAll[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], True, And]
type Exists[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], False, Or]

type Inverse[P[_]] = [X] =>> Not[P[X]]
