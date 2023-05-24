package constraints

/**
 * Type class instance for [[ValueOf]] of [[NonEmptyTuple]]s
 * @tparam H the type of the first element in the tuple
 * @tparam T the type of the remaining elements in the tuple
 * @return the runtime value of the tuple type
 */
given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])