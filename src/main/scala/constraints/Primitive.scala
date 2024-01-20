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

  /**
   * instance for lifting a primitive value to its literally (narrowly) typed value
   * @param primitive the value to inline
   * @param Quotes for performing macro operations
   * @tparam P The specific type of the value to lift
   * @return the value as an expression
   */
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
