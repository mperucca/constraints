package constraint

given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])
