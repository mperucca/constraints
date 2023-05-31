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

def tupleToType(tuple: Tuple)(using Quotes): quoted.quotes.reflect.TypeRepr =
  import quoted.quotes.reflect.*
  (tuple: Tuple) match
    case EmptyTuple => TypeRepr.of[EmptyTuple]
    case h *: t =>
      given Extractable[h.type] = null.asInstanceOf
      val head = Extractable.toType[h.type](h)
      AppliedType(TypeRepr.of[*:[_, _]], List(head, tupleToType(t)))

def tupleToExpr[T <: Tuple : Extractable](tuple: T)(using Quotes): Expr[T] =
  val expr = tuple match
    case EmptyTuple => ToExpr.EmptyTupleToExpr(EmptyTuple)
    case h *: t =>
      val head = h match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Extractable[t.type] = null.asInstanceOf
          tupleToExpr[t.type](t)
      given Extractable[t.type] = null.asInstanceOf
      val tail = tupleToExpr[t.type](t)
      '{ $head *: $tail }
  expr.asInstanceOf[Expr[T]]