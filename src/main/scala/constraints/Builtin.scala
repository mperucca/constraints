package constraints

import scala.quoted.*

sealed trait Builtin[E]

object Builtin:

  given[P <: Primitive]: Builtin[P] = new Builtin[P] {}

  given[E <: EmptyTuple]: Builtin[E] = new Builtin[E] {}

  given[N <: H *: T, H: Builtin, T <: Tuple : Builtin]: Builtin[N] = new Builtin[N] {}

  def evidenceOrAbort[E: Type](using Quotes): Builtin[E] =
    Expr.summon[Builtin[E]] match
      case None =>
        import quoted.quotes.reflect.*
        report.errorAndAbort("cannot extract value from type " + TypeRepr.of[E].show)
      case Some(extractable) => new Builtin[E] {}

  given extractable[B: Builtin : Type] (using Quotes): Extractable[B] with
    override def extract: Option[B] =
      unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

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
   * Lift an extractable value to its literal type
   */
  def toLiteralType[E: Builtin](extractable: E)(using Quotes): quoted.quotes.reflect.TypeRepr =
    extractable match
      case p: Primitive => Primitive.toConstantType(p)
      case t: Tuple => tupleToLiteralTupleType(t)

  /**
   * Lift an extractable value to its literal expression
   */
  def toExpr[E: Builtin: Type](extractable: E)(using Quotes): Expr[E] =
    val expr = extractable match
      case p: Primitive => Primitive.toExpr(p)
      case t: Tuple =>
        given Builtin[t.type] = null.asInstanceOf
        tupleToExpr[t.type](t)
    val tpe = toLiteralType(extractable)
    tpe.asType match
      case '[e] =>
        val typedExpr = '{ $expr.asInstanceOf[e] } // asInstanceOf needed to further reduce inlining
        typedExpr.asExprOf[E]
