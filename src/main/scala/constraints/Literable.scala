package constraints

import scala.quoted.*

trait Literable[V]:

  def toLiteral(value: V)(using Quotes): Expr[V]

object Literable:

  given [V: Builtin: Type]: Literable[V] with

    override def toLiteral(value: V)(using Quotes): Expr[V] =
      val expr = value match
        case p: Primitive => Primitive.toExpr(p)
        case t: Tuple =>
          given Builtin[t.type] = Builtin[t.type]
          tupleToExpr[t.type](t)
      expr

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
