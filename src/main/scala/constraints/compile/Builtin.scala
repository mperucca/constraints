package constraints.compile

import scala.quoted.*

/**
 * Evidence for the types natively supported from which values can be extracted.
 * The reason for this not being a union type is because a [[NonEmptyTuple]] can have values
 * extracted from it only if it is made up entirely of [[Builtin]]s which may be recursive.
 * @tparam E the type
 */
class Builtin[E] private[constraints]()

object Builtin:

  /**
   * Evidence for [[Primitive]] values and the [[EmptyTuple]] value
   * @tparam P The specific subtype of the [[Primitive]] or [[EmptyTuple]]
   * @return evidence for the [[Builtin]] type
   */
  given[P <: Primitive | EmptyTuple]: Builtin[P] = Builtin[P]

  /**
   * Evidence for [[NonEmptyTuple]]s as long as it's made up of [[Builtin]]s
   * @tparam N The specific subtype of the [[NonEmptyTuple]]
   * @tparam H The specific subtype of the head of the tuple
   * @tparam T The specific subtype of the tail of the tuple
   * @return evidence for the [[Builtin]] type
   */
  given[N <: H *: T, H: Builtin, T <: Tuple : Builtin]: Builtin[N] = Builtin[N]

  /**
   * Either summons evidence that [[E]] is [[Builtin]] or fails at compile time
   * @param Quotes performs operations in macro contexts
   * @tparam E The type to assert as a [[Builtin]] type
   * @return The [[Builtin]] evidence
   */
  def evidenceOrAbort[E: Type](using Quotes): Builtin[E] =
    Expr.summon[Builtin[E]] match
      case None =>
        import quoted.quotes.reflect.*
        report.errorAndAbort("cannot extract value from type " + TypeRepr.of[E].show)
      case Some(extractable) => Builtin[E]

  /**
   * Lifts a [[Builtin]] type to its [[Expr]] value which is narrowly typed (not widened)
   */
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
