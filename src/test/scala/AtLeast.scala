import constraints.{CompileTimeComputation, RuntimeComputation}

import scala.quoted.{Expr, Quotes, Type}

type AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[
    Value <: Orderable: ValueOf,
    Minimum <: Orderable: ValueOf,
    Orderable: Ordering
  ]: RuntimeComputation.Predicate[Value AtLeast Minimum] =
    RuntimeComputation(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  given compileTimeCheckDouble[
    Value <: Double,
    Minimum <: Double
  ]: CompileTimeComputation[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implDouble[Value, Minimum] }

  given compileTimeCheckInt[
    Value <: Int,
    Minimum <: Int
  ]: CompileTimeComputation[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implInt[Value, Minimum] }

  given compileTimeCheckString[
    Value <: String,
    Minimum <: String
  ]: CompileTimeComputation[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implString[Value, Minimum] }

  given compileTimeCheckChar[
    Value <: Char,
    Minimum <: Char
  ]: CompileTimeComputation[Value AtLeast Minimum] with
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implChar[Value, Minimum] }

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
    CompileTimeComputation.fromRuntimePostponingExtractableCheck[(Value, Minimum), Boolean] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Orderable]
    }

