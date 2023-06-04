package constraints

import scala.quoted.*

trait Literable[V]:

  def toLiteralType(value: V): Type[V]
  def toLiteralExpr(value: V): Expr[V]

object Literable:

  given [V: Builtin: Type](using Quotes): Literable[V] with
    override def toLiteralType(value: V): Type[V] =
      val tpe = value match
        case p: Primitive => Primitive.toConstantType(p)
        case t: Tuple => tupleToLiteralTupleType(t)
      tpe.asType.asInstanceOf[Type[V]]

    override def toLiteralExpr(value: V): Expr[V] =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = null.asInstanceOf
          tupleToExpr[t.type](t)
      val tpe = toLiteralType(value)
      tpe match
        case '[v] =>
          val typedExpr = '{ $expr.asInstanceOf[v] } // asInstanceOf needed to further reduce inlining
          typedExpr.asExprOf[V]

  given [N <: H *: T : Type, H <: Singleton: Literable: Type, T <: Tuple: Literable: Type](using Quotes): Literable[N] with
    override def toLiteralType(value: N): Type[N] = Type.of[N]
    override def toLiteralExpr(value: N): Expr[N] =
      val headExpr = summon[Literable[H]].toLiteralExpr(value.head.asInstanceOf[H])
      val tailExpr = summon[Literable[T]].toLiteralExpr(value.tail.asInstanceOf[T])
      '{ $headExpr *: $tailExpr }.asExprOf[N] // asInstanceOf needed to further reduce inlining
