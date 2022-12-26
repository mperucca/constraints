package constraints

import compiletime.*
import quoted.*

given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])

inline def constValueRecursive[T]: T =
  val res = inline erasedValue[T] match
    case _: EmptyTuple => EmptyTuple
    case _: (EmptyTuple *: ts) => EmptyTuple *: constValueRecursive[ts]
    case _: ((t *: hts) *: ts) => constValueRecursive[t *: hts] *: constValueRecursive[ts]
    case _: (t *: ts) => constValue[t] *: constValueRecursive[ts]
  res.asInstanceOf[T]

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
