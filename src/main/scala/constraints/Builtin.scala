package constraints

import scala.quoted.*

class Builtin[E] private[constraints]()

object Builtin:

  given[P <: Primitive | EmptyTuple]: Builtin[P] = Builtin[P]

  given[N <: H *: T, H: Builtin, T <: Tuple : Builtin]: Builtin[N] = Builtin[N]

  def evidenceOrAbort[E: Type](using Quotes): Builtin[E] =
    Expr.summon[Builtin[E]] match
      case None =>
        import quoted.quotes.reflect.*
        report.errorAndAbort("cannot extract value from type " + TypeRepr.of[E].show)
      case Some(extractable) => Builtin[E]

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

  given toExpr[B: Builtin: Type]: ToExpr[B] with
    override def apply(builtin: B)(using Quotes): Expr[B] =
      val expr = builtin match
        case primitive: Primitive => Primitive.toExpr(primitive)
        case tuple: Tuple =>
          given Builtin[tuple.type] = Builtin[tuple.type]
          tupleToExpr[tuple.type](tuple)
      val tpe = ToType(builtin)
      tpe match
        case '[b] =>
          val typedExpr = '{ $expr.asInstanceOf[b] } // asInstanceOf needed to further reduce inlining
          typedExpr.asExprOf[B]
