import constraints.Compute
import constraints.compile.Inlinable

import scala.quoted.{Expr, Quotes, Type}

infix type AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[
    Value <: Orderable: ValueOf,
    Minimum <: Orderable: ValueOf,
    Orderable: Ordering
  ]: Compute.Typed[Value AtLeast Minimum, Boolean] =
    Compute(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  given compileTimeCheckDouble[
    Value <: Double,
    Minimum <: Double
  ]: Inlinable[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${ implDouble[Value, Minimum] }

  given compileTimeCheckInt[
    Value <: Int,
    Minimum <: Int
  ]: Inlinable[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${ implInt[Value, Minimum] }

  given compileTimeCheckString[
    Value <: String,
    Minimum <: String
  ]: Inlinable[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${ implString[Value, Minimum] }

  given compileTimeCheckChar[
    Value <: Char,
    Minimum <: Char
  ]: Inlinable[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${ implChar[Value, Minimum] }

  private def implDouble[
    Value <: Double : Type,
    Minimum <: Double : Type
  ](using Quotes): Expr[Option[Boolean]] =
    impl[Value, Minimum, Double]

  private def implInt[
    Value <: Int : Type,
    Minimum <: Int : Type
  ](using Quotes): Expr[Option[Boolean]] =
    impl[Value, Minimum, Int]

  private def implString[
    Value <: String: Type,
    Minimum <: String: Type
  ](using Quotes): Expr[Option[Boolean]] =
    impl[Value, Minimum, String]

  private def implChar[
    Value <: Char: Type,
    Minimum <: Char: Type
  ](using Quotes): Expr[Option[Boolean]] =
    impl[Value, Minimum, Char]

  private def impl[
    Value <: Orderable: Type,
    Minimum <: Orderable: Type,
    Orderable: Ordering
  ](using Quotes): Expr[Option[Boolean]] =
    Inlinable.fromComputationPostponingExtractableCheck[(Value, Minimum), Boolean] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Orderable]
    }

