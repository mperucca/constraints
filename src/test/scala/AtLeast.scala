import constraints.{CompileTimeCheck, CompileTimeComputation, Extractable, RuntimeCheck}

import scala.quoted.{Expr, Quotes, Type}

sealed trait AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[Value <: Orderable: ValueOf, Minimum <: Orderable: ValueOf, Orderable: Ordering]: RuntimeCheck[Value AtLeast Minimum] =
    RuntimeCheck(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  transparent inline given compileTimeCheckDouble[Value <: Double, Minimum <: Double]: CompileTimeComputation.Typed[Value AtLeast Minimum, Boolean] =
    CompileTimeCheckDouble[Value, Minimum]

  transparent inline given compileTimeCheckInt[Value <: Int, Minimum <: Int]: CompileTimeComputation.Typed[Value AtLeast Minimum, Boolean] =
    CompileTimeCheckInt[Value, Minimum]

  transparent inline given compileTimeCheckString[Value <: String, Minimum <: String]: CompileTimeComputation.Typed[Value AtLeast Minimum, Boolean] =
    CompileTimeCheckString[Value, Minimum]

  transparent inline given compileTimeCheckChar[Value <: Char, Minimum <: Char]: CompileTimeComputation.Typed[Value AtLeast Minimum, Boolean] =
    CompileTimeCheckChar[Value, Minimum]

  class CompileTimeCheckDouble[Value <: Double, Minimum <: Double] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${ implDouble[Value, Minimum] }

  class CompileTimeCheckChar[Value <: Char, Minimum <: Char] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${ implChar[Value, Minimum] }

  class CompileTimeCheckInt[Value <: Int, Minimum <: Int] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${ implInt[Value, Minimum] }

  class CompileTimeCheckString[Value <: String, Minimum <: String] extends CompileTimeComputation[Value AtLeast Minimum]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${ implString[Value, Minimum] }

  private def implDouble[Value <: Double : Type, Minimum <: Double : Type](using Quotes): Expr[false | Null | true] =
    impl[Value, Minimum, Double]

  private def implInt[Value <: Int : Type, Minimum <: Int : Type](using Quotes): Expr[false | Null | true] =
    impl[Value, Minimum, Int]

  private def implString[Value <: String: Type, Minimum <: String: Type](using Quotes): Expr[false | Null | true] =
    impl[Value, Minimum, String]

  private def implChar[Value <: Char: Type, Minimum <: Char: Type](using Quotes): Expr[false | Null | true] =
    impl[Value, Minimum, Char]

  private def impl[Value <: Orderable: Type, Minimum <: Orderable: Type, Orderable <: Extractable: Ordering](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnTuple[(Value, Minimum)] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Orderable]
    }

