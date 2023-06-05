package constraints

import scala.quoted.*

trait Literable[V]:

  def toLiteral(value: V)(using Quotes): (Expr[V], quoted.quotes.reflect.TypeRepr)

object Literable:

  given [V: Builtin: Type]: Literable[V] with

    override def toLiteral(value: V)(using Quotes): (Expr[V], quoted.quotes.reflect.TypeRepr) =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = Builtin[t.type]
          tupleToExpr[t.type](t)
      val tpe = toLiteralType(value)
      tpe.asType match
        case '[v] =>
          val typedExpr = '{ $expr.asInstanceOf[v] } // asInstanceOf needed to further reduce inlining
          (typedExpr.asExprOf[V], tpe)

    def toLiteralType(value: V)(using Quotes): quoted.quotes.reflect.TypeRepr =
      value match
        case p: Primitive => Primitive.toConstantType(p)
        case t: Tuple => tupleToLiteralTupleType(t)

    def toLiteralExpr(value: V)(using Quotes): Expr[V] =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = Builtin[t.type]
          tupleToExpr[t.type](t)
      expr.asExprOf[V]
