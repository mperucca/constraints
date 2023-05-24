package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
sealed trait Extractable[-E]

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

object Primitive:

  given toExpr[P <: Primitive: Type]: ToExpr[P] with
    def apply(primitive: P)(using Quotes): Expr[P] =
      val expr = (primitive: Primitive) match
        case b: Boolean => ToExpr.BooleanToExpr(b)
        case b: Byte => ToExpr.ByteToExpr(b)
        case s: Short => ToExpr.ShortToExpr(s)
        case i: Int => ToExpr.IntToExpr(i)
        case l: Long => ToExpr.LongToExpr(l)
        case f: Float => ToExpr.FloatToExpr(f)
        case d: Double => ToExpr.DoubleToExpr(d)
        case c: Char => ToExpr.CharToExpr(c)
        case s: String => ToExpr.StringToExpr(s)
      expr.asInstanceOf[Expr[P]]

/**
 * Holds the extraction method
 */
object Extractable:

  def evidenceOrAbort[E: Type](using Quotes): Extractable[E] =
    Expr.summon[Extractable[E]] match
      case None =>
        import quoted.quotes.reflect.*
        report.errorAndAbort("cannot extract value from type " + TypeRepr.of[E].show)
      case Some(extractable) => new Extractable[E] {}

  given Extractable[Primitive] = new Extractable[Primitive] {}

  given Extractable[EmptyTuple] = new Extractable[EmptyTuple] {}

  given [H, T <: Tuple](using Extractable[H], Extractable[T]): Extractable[H *: T] = new Extractable[H *: T] {}

  /**
   * Extracts the value of a constant type during macro expansion, possible recursively from nested [[Group]]s
   *
   * @param Quotes for macro operations
   * @tparam V the type to extract the value from
   * @return [[Some]] value if it can be extracted from the constant type or [[None]] otherwise
   */
  def extract[V : Type: Extractable](using Quotes): Option[V] =
    Extract.unapply(quotes.reflect.TypeRepr.of[V]).map(_.asInstanceOf[V])

  private object Extract:
    def unapply(using Quotes)(tpe: quotes.reflect.TypeRepr): Option[Any] =
      import quotes.reflect.*
      tpe.widenTermRefByName.dealias.simplified match
        case ConstantType(const) => Some(const.value)
        case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
          tpes.foldRight(Option[Tuple](EmptyTuple)) {
            case (_, None) => None
            case (Extract(v), Some(acc)) => Some(v *: acc)
            case _ => None
          }
        case AppliedType(tp, List(Extract(headValue), tail)) if tp =:= TypeRepr.of[*:] =>
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
        case tpe =>
          Option.when(tpe =:= TypeRepr.of[EmptyTuple])(EmptyTuple)

  /**
   * Lift an extractable value to its literal expression
   */
  given toExpr[E: Extractable]: ToExpr[E] with
    def apply(extractable: E)(using Quotes): Expr[E] =
      val expr = extractable match
        case p: Primitive => Primitive.toExpr[Primitive].apply(p)
        case EmptyTuple => ToExpr.EmptyTupleToExpr(EmptyTuple)
        case h *: t =>
          given Extractable[h.type] = null.asInstanceOf
          given Extractable[t.type] = null.asInstanceOf
          val he = toExpr[h.type].apply(h)
          val te = toExpr[t.type].apply(t)
          '{${he} *: ${te}}
      expr.asInstanceOf[Expr[E]]