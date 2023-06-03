package constraints

import scala.quoted.*

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

  def toConstantType(primitive: Primitive)(using Quotes): quoted.quotes.reflect.ConstantType =
    import quoted.quotes.reflect.*
    val constant = primitive match
      case b: Boolean => BooleanConstant(b)
      case b: Byte => ByteConstant(b)
      case s: Short => ShortConstant(s)
      case i: Int => IntConstant(i)
      case l: Long => LongConstant(l)
      case f: Float => FloatConstant(f)
      case d: Double => DoubleConstant(d)
      case c: Char => CharConstant(c)
      case s: String => StringConstant(s)
    ConstantType(constant)

  def toExpr[P <: Primitive](primitive: P)(using Quotes): Expr[P] =
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
