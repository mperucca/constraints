package constraints

import compiletime.*
import quoted.*

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

/**
 * Extracts the value of a constant type during macro expansion, possible recursively from nested tuples
 * @param Type[T] the reified type to extract the value from
 * @param Quotes for macro operations
 * @tparam T the type to extract the value from
 * @return [[Some]] value if it can be extracted from the constant type or [[None]] otherwise
 */
def valueOfConstantRecursive[T](using Type[T])(using Quotes): Option[T] =
  ValueOfConstantRecursive.unapply(quotes.reflect.TypeRepr.of[T]).asInstanceOf[Option[T]]

private object ValueOfConstantRecursive:
  def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Any] =
    import quotes.reflect.*
    tpe.widenTermRefByName.dealias match
      case ConstantType(const) => Some(const.value)
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes.foldRight(Option[Tuple](EmptyTuple)) {
          case (_, None) => None
          case (ValueOfConstantRecursive(v), Some(acc)) => Some(v *: acc)
          case _ => None
        }
      case AppliedType(tp, List(ValueOfConstantRecursive(headValue), tail)) if tp =:= TypeRepr.of[*:] =>
        unapply(tail) match
          case Some(tailValue) => Some(headValue *: tailValue.asInstanceOf[Tuple])
          case None => None
      case intersectionType @ AndType(tp1, tp2) =>
        (unapply(tp1), unapply(tp2)) match
          case (None, None) => None
          case (None, v: Some[Any]) => v
          case (v: Some[Any], None) => v
          case (Some(v1), Some(v2)) =>
            if v1 != v2 then
              report.errorAndAbort(s"intersection type ${intersectionType.show} produced two values: $v1 and $v2")
            else Some(v1)
      case tpe =>
        Option.when(tpe =:= TypeRepr.of[EmptyTuple])(EmptyTuple)
