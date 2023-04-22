package constraints

import compiletime.*

/**
 * Type class instance for [[ValueOf]] of [[NonEmptyTuple]]s
 * @tparam H the type of the first element in the tuple
 * @tparam T the type of the remaining elements in the tuple
 * @return the runtime value of the tuple type
 */
given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])

/**
 * Extracts the value of a constant type, possibly recursively from nested tuples,
 * failing compilation if it cannot be extracted
 * @tparam T the type to extract the value from
 * @return the value of the constant type
 */
inline def constValueRecursive[T]: T =
  val res = inline erasedValue[T] match
    case EmptyTuple => EmptyTuple
    case _: (EmptyTuple *: ts) => EmptyTuple *: constValueRecursive[ts]
    case _: ((t *: hts) *: ts) => constValueRecursive[t *: hts] *: constValueRecursive[ts]
    case _: (t *: ts) => constValue[t] *: constValueRecursive[ts]
  res.asInstanceOf[T]
