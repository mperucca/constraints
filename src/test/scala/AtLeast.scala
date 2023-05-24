import constraints.{CompileTimeComputation, Extractable, RuntimeComputation}

import scala.quoted.{Expr, Quotes, Type}

type AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[
    Value <: Orderable: ValueOf,
    Minimum <: Orderable: ValueOf,
    Orderable: Ordering
  ]: RuntimeComputation.Predicate[Value AtLeast Minimum] =
    RuntimeComputation(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  transparent inline given compileTimeCheckDouble[
    Value <: Double,
    Minimum <: Double
  ]: CompileTimeComputation.Predicate[Value AtLeast Minimum] =
    CompileTimeCheckDouble[Value, Minimum]

  transparent inline given compileTimeCheckInt[
    Value <: Int,
    Minimum <: Int
  ]: CompileTimeComputation.Predicate[Value AtLeast Minimum] =
    CompileTimeCheckInt[Value, Minimum]

  transparent inline given compileTimeCheckString[
    Value <: String,
    Minimum <: String
  ]: CompileTimeComputation.Predicate[Value AtLeast Minimum] =
    CompileTimeCheckString[Value, Minimum]

  transparent inline given compileTimeCheckChar[
    Value <: Char,
    Minimum <: Char
  ]: CompileTimeComputation.Predicate[Value AtLeast Minimum] =
    CompileTimeCheckChar[Value, Minimum]

  class CompileTimeCheckDouble[
    Value <: Double,
    Minimum <: Double
  ] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implDouble[Value, Minimum] }

  class CompileTimeCheckChar[
    Value <: Char,
    Minimum <: Char
  ] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implChar[Value, Minimum] }

  class CompileTimeCheckInt[
    Value <: Int,
    Minimum <: Int
  ] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implInt[Value, Minimum] }

  class CompileTimeCheckString[
    Value <: String,
    Minimum <: String
  ] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implString[Value, Minimum] }

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
    Orderable <: Extractable: Ordering
  ](using Quotes): Expr[Boolean | Null] =
    CompileTimeComputation.fromRuntimeOnTuple[(Value, Minimum), Boolean] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Orderable]
    }

