package constraints

import scala.quoted.*

/**
 * Type class instance for [[ValueOf]] of [[NonEmptyTuple]]s
 *
 * @tparam H the type of the first element in the tuple
 * @tparam T the type of the remaining elements in the tuple
 * @return the runtime value of the tuple type
 */
given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])

def tupleToExpr[T <: Tuple : Builtin](tuple: T)(using Quotes): Expr[T] =
  val expr = tuple match
    case EmptyTuple => ToExpr.EmptyTupleToExpr(EmptyTuple)
    case h *: t =>
      val head = h match
        case primitive: Primitive => Primitive.toExpr(primitive)
        case tuple: Tuple =>
          given Builtin[tuple.type] = Builtin[tuple.type]
          tupleToExpr[tuple.type](tuple)
      given Builtin[t.type] = Builtin[t.type]
      val tail = tupleToExpr[t.type](t)
      '{ $head *: $tail }
  expr.asInstanceOf[Expr[T]]