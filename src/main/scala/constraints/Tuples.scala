package constraints

import compiletime.*
import quoted.*

given nonEmptyTupleValueOf[H: ValueOf, T <: Tuple: ValueOf]: ValueOf[H *: T] = ValueOf(valueOf[H] *: valueOf[T])

inline def constValueTupleRecursive[T <: Tuple]: T =
  val res = inline erasedValue[T] match
    case _: EmptyTuple => EmptyTuple
    case _: (EmptyTuple *: ts) => EmptyTuple *: constValueTupleRecursive[ts]
    case _: ((t *: hts) *: ts) => constValueTupleRecursive[t *: hts] *: constValueTupleRecursive[ts]
    case _: (t *: ts) => constValue[t] *: constValueTupleRecursive[ts]
  res.asInstanceOf[T]

def valueOfTupleRecursive[T <: Tuple](using Type[T])(using Quotes): Option[T] =
  ValueOfTupleRecursive.unapply(quotes.reflect.TypeRepr.of[T]).asInstanceOf[Option[T]]

private object ValueOfTupleRecursive:
  def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Any] =
    import quotes.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    tpe.widenTermRefByName.dealias match
      case ConstantType(const) => Some(const.value)
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes.foldRight(Option[Tuple](EmptyTuple)) {
          case (_, None) => None
          case (ValueOfTupleRecursive(v), Some(acc)) => Some(v *: acc)
          case _ => None
        }
      case AppliedType(tp, List(ValueOfTupleRecursive(headValue), tail)) if tp.derivesFrom(cons) =>
        unapply(tail) match
          case Some(tailValue) => Some(headValue *: tailValue.asInstanceOf[Tuple])
          case None => None
      case tpe =>
        if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then Some(EmptyTuple)
        else None