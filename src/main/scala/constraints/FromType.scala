package constraints

import scala.quoted.*

/**
 * Type class for extracting values from a type at compile time
 * @tparam E The type to extract a value from
 */
trait FromType[E]:

  /**
   * Attempts to extract a value from the type [[E]]
   * @param Quotes for performing macro operations
   * @return the possibly extracted value
   */
  def extract(using Quotes): Option[E]

object FromType:

  /**
   * Attempts to extract a value from type [[E]]
   * @param fromType the type class instance for extracting the value
   * @param Quotes for performing macro operations
   * @tparam E the type to extract a value from
   * @return the possibly extracted value
   */
  def apply[E](using fromType: FromType[E])(using Quotes): Option[E] = fromType.extract

  /**
   * instance for extracting values from a literal builtin type
   */
  given builtinSingleton[B <: Singleton: Builtin : Type]: FromType[B] with
    override def extract(using Quotes): Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  /**
   * instance for extracting values from a widened builtin type
   */
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

  /**
   * instance for extracting values from a tuple made up of extractable parts
   */
  given nonEmptyTuple[H, T <: Tuple](using h: FromType[H], t: FromType[T]): FromType[H *: T] with
    override def extract(using Quotes): Option[H *: T] =
      for head <- h.extract
          tail <- t.extract
        yield head *: tail

  given option[O <: Option[A]: Type, A](using a: FromType[A]): FromType[O] with
    override def extract(using Quotes): Option[O] =
      import quotes.reflect.*
      val tpe = TypeRepr.of[O]
      if (tpe <:< TypeRepr.of[None.type]) Some(None.asInstanceOf[O])
      else if (tpe <:< TypeRepr.of[Some[Any]]) a.extract.map(Some(_).asInstanceOf[O])
      else None
