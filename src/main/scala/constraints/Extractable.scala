package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
type Extractable = Primitive | Group

/**
 * The primitive types built in to Scala
 */
type Primitive =
  String
    | Boolean
    | Byte
    | Char
    | Double
    | Float
    | Int
    | Long
    | Short

/**
 * Holds the extraction method
 */
object Extractable:

  /**
   * Extracts the value of a constant type during macro expansion, possible recursively from nested [[Group]]s
   *
   * @param Quotes for macro operations
   * @tparam V the type to extract the value from
   * @return [[Some]] value if it can be extracted from the constant type or [[None]] otherwise
   */
  def extract[V <: Extractable : Type](using Quotes): Option[V] =
    Extract.unapply(quotes.reflect.TypeRepr.of[V]).map(_.asInstanceOf[V])

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
              if v1 != v2
              then report.errorAndAbort(s"intersection type ${intersectionType.show} produced two values: $v1 and $v2")
              else Some(v1)
        case tpe =>
          Option.when(tpe =:= TypeRepr.of[Group.End.type])(Group.End)

  given toExpr[E <: Extractable]: ToExpr[E] with
    def apply(extractable: E)(using Quotes): Expr[E] =
      val expr = (extractable: Extractable) match
        case b: Boolean => ToExpr.BooleanToExpr(b)
        case b: Byte => ToExpr.ByteToExpr(b)
        case s: Short => ToExpr.ShortToExpr(s)
        case i: Int => ToExpr.IntToExpr(i)
        case l: Long => ToExpr.LongToExpr(l)
        case f: Float => ToExpr.FloatToExpr(f)
        case d: Double => ToExpr.DoubleToExpr(d)
        case c: Char => ToExpr.CharToExpr(c)
        case s: String => ToExpr.StringToExpr(s)
        case Group.End => Group.End.toExpr
        case Group.Link(h, t) => Group.Link.toExpr[Extractable, Group].apply(Group.Link(h, t))
      (expr: Expr[Extractable]).asInstanceOf[Expr[E]]