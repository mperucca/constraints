import constraints.{Inliner, Computation}

import scala.quoted.{Expr, Quotes, Type}

type AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[
    Value <: Orderable: ValueOf,
    Minimum <: Orderable: ValueOf,
    Orderable: Ordering
  ]: Computation.Predicate[Value AtLeast Minimum] =
    Computation(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  given compileTimeCheckDouble[
    Value <: Double,
    Minimum <: Double
  ]: Inliner[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${ implDouble[Value, Minimum] }

  given compileTimeCheckInt[
    Value <: Int,
    Minimum <: Int
  ]: Inliner[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${ implInt[Value, Minimum] }

  given compileTimeCheckString[
    Value <: String,
    Minimum <: String
  ]: Inliner[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${ implString[Value, Minimum] }

  given compileTimeCheckChar[
    Value <: Char,
    Minimum <: Char
  ]: Inliner[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${ implChar[Value, Minimum] }

  private def implDouble[
    Value <: Double : Type,
    Minimum <: Double : Type
  ](using Quotes): Expr[Boolean | Null] =
    impl[Value, Minimum, Double]

  private def implInt[
    Value <: Int : Type,
    Minimum <: Int : Type
  ](using Quotes): Expr[Boolean | Null] =
    impl[Value, Minimum, Int]

  private def implString[
    Value <: String: Type,
    Minimum <: String: Type
  ](using Quotes): Expr[Boolean | Null] =
    impl[Value, Minimum, String]

  private def implChar[
    Value <: Char: Type,
    Minimum <: Char: Type
  ](using Quotes): Expr[Boolean | Null] =
    impl[Value, Minimum, Char]

  private def impl[
    Value <: Orderable: Type,
    Minimum <: Orderable: Type,
    Orderable: Ordering
  ](using Quotes): Expr[Boolean | Null] =
    Inliner.fromComputationPostponingExtractableCheck[(Value, Minimum), Boolean] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Orderable]
    }

