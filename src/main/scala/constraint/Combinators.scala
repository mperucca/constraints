package constraint

sealed trait And[A, B]
sealed trait Not[A]
sealed trait Or[A, B]
sealed trait Xor[A, B]
sealed trait True
sealed trait False

type Implies[A, B] = Not[A] Or B
type Nand[A, B] = Not[A And B]
type Nor[A, B] = Not[A Or B]
type Xnor[A, B] = Not[A Xor B]
type ForAll[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], True, And]
type Exists[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], False, Or]
