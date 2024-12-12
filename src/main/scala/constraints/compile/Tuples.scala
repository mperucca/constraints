package constraints.compile

import scala.quoted.*

/**
 * Lifts a tuple value into it's narrowed expression type
 * @param tuple the tuple to lift to an expression
 * @param Quotes for performing macro operations
 * @tparam T The specific tuple typle
 * @return the liften tuple expression
 */
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