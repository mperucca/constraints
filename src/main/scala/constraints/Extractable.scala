package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
sealed trait Extractable[-E]

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
        case Refinement(tp, _, _) => unapply(tp)
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
   * Lift an extractable value to its literal type
   */
  def toType[E: Extractable](extractable: E)(using Quotes): quoted.quotes.reflect.TypeRepr =
    extractable match
      case p: Primitive => Primitive.toType(p)
      case t: Tuple => tupleToType(t)

  /**
   * Lift an extractable value to its literal expression
   */
  def toExpr[E: Extractable: Type](extractable: E)(using Quotes): Expr[E] =
    val expr = extractable match
      case p: Primitive => Primitive.toExpr(p)
      case t: Tuple =>
        given Extractable[t.type] = null.asInstanceOf
        tupleToExpr[t.type](t)
    val tpe = toType(extractable)
    tpe.asType match
      case '[e] =>
        val typedExpr = '{ $expr.asInstanceOf[e] } // asInstanceOf needed to further reduce inlining
        typedExpr.asExprOf[E]
