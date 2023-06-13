package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
trait FromType[E]:

  def extract(using Quotes): Option[E]

/**
 * Holds the extraction method
 */
object FromType:

  def apply[E](using fromType: FromType[E])(using Quotes): Option[E] = fromType.extract

  given builtinSingleton[B <: Singleton: Builtin : Type]: FromType[B] with
    override def extract(using Quotes): Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  given builtin[B: Builtin : Type]: FromType[B] with
    override def extract(using Quotes): Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  private object Builtin:
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Any] =
      import quotes.reflect.*
      tpe.widenTermRefByName.dealias.simplified match
        case ConstantType(const) => Some(const.value)
        case Refinement(tp, _, _) => unapply(tp)
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(Option[Tuple](EmptyTuple)) {
            case (_, None) => None
            case (Builtin(v), Some(acc)) => Some(v *: acc)
            case _ => None
          }
        case AppliedType(tp, List(Builtin(headValue), tail)) if tp =:= TypeRepr.of[*:] =>
          unapply(tail) match
            case Some(tailValue) => Some(headValue *: tailValue.asInstanceOf[Tuple])
            case None => None
        case intersectionType@AndType(tp1, tp2) =>
          (unapply(tp1), unapply(tp2)) match
            case (None, None) => None
            case (None, v: Some[Any]) => v
            case (v: Some[Any], None) => v
            case (Some(v1), Some(v2)) =>
              if v1 != v2
              then report.errorAndAbort(s"intersection type ${intersectionType.show} produced two values: $v1 and $v2")
              else Some(v1)
        case tp =>
          Option.when(tp =:= TypeRepr.of[EmptyTuple])(EmptyTuple)

  given nonEmptyTuple[H, T <: Tuple](using h: FromType[H], t: FromType[T]): FromType[H *: T] with
    override def extract(using Quotes): Option[H *: T] =
      for head <- h.extract
          tail <- t.extract
        yield head *: tail
