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
