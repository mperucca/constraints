package constraints

import scala.quoted.*

trait Literable[V]:

  def toLiteral(value: V): (Expr[V], Type[V])

object Literable:

  given [V: Builtin: Type](using Quotes): Literable[V] with

    override def toLiteral(value: V): (Expr[V], Type[V]) =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = Builtin[t.type]
          tupleToExpr[t.type](t)
      val tpe = toLiteralType(value)
      tpe match
        case '[v] =>
          val typedExpr = '{ $expr.asInstanceOf[v] } // asInstanceOf needed to further reduce inlining
          (typedExpr.asExprOf[V], tpe)

    def toLiteralType(value: V): Type[V] =
      val tpe = value match
        case p: Primitive => Primitive.toConstantType(p)
        case t: Tuple => tupleToLiteralTupleType(t)
      tpe.asType.asInstanceOf[Type[V]]

    def toLiteralExpr(value: V): Expr[V] =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = Builtin[t.type]
          tupleToExpr[t.type](t)
      expr.asExprOf[V]
