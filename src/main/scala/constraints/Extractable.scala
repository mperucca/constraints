package constraints

import scala.quoted.*

type Extractable = Primitive | Group

type Primitive = Boolean | Byte | Short | Int | Long | Float | Double | Char | String

object Extractable:

  /**
   * Extracts the value of a constant type during macro expansion, possible recursively from nested [[Group]]s
   *
   * @param Quotes for macro operations
   * @tparam T the type to extract the value from
   * @return [[Some]] value if it can be extracted from the constant type or [[None]] otherwise
   */
  def extract[T <: Extractable : Type](using Quotes): Option[T] =
    Extract.unapply(quotes.reflect.TypeRepr.of[T]).map(_.asInstanceOf[T])

  private object Extract:
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Extractable] =
      import quotes.reflect.*
      tpe.widenTermRefByName.dealias.simplified match
        case ConstantType(const) => Some(const.value.asInstanceOf[Primitive])
        case AppliedType(tp, List(Extract(headValue), tail)) if tp =:= TypeRepr.of[Group.Link] =>
          unapply(tail) match
            case Some(tailValue) => Some(Group.Link(headValue, tailValue.asInstanceOf[Group]))
            case None => None
        case intersectionType@AndType(tp1, tp2) =>
          (unapply(tp1), unapply(tp2)) match
            case (None, None) => None
            case (None, v: Some[Any]) => v
            case (v: Some[Any], None) => v
            case (Some(v1), Some(v2)) =>
              if v1 != v2 then
                report.errorAndAbort(s"intersection type ${intersectionType.show} produced two values: $v1 and $v2")
              else Some(v1)
        case tpe =>
          Option.when(tpe =:= TypeRepr.of[Group.End.type])(Group.End)
