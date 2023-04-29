import constraints.{CompileTimeCheck, Extractable, RuntimeCheck}

import scala.quoted.{Expr, Quotes, Type}

sealed trait AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[Value <: Orderable: ValueOf, Minimum <: Orderable: ValueOf, Orderable: Ordering]: RuntimeCheck[Value AtLeast Minimum] =
    RuntimeCheck(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  transparent inline given compileTimeCheckDouble[Value <: Double, Minimum <: Double]: CompileTimeCheck[Value AtLeast Minimum] =
    CompileTimeCheckDouble[Value, Minimum]

  transparent inline given compileTimeCheckInt[Value <: Int, Minimum <: Int]: CompileTimeCheck[Value AtLeast Minimum] =
    CompileTimeCheckInt[Value, Minimum]

  transparent inline given compileTimeCheckString[Value <: String, Minimum <: String]: CompileTimeCheck[Value AtLeast Minimum] =
    CompileTimeCheckString[Value, Minimum]

  transparent inline given compileTimeCheckChar[Value <: Char, Minimum <: Char]: CompileTimeCheck[Value AtLeast Minimum] =
    CompileTimeCheckChar[Value, Minimum]

  private class CompileTimeCheckDouble[Value <: Double, Minimum <: Double] extends CompileTimeCheck[Value AtLeast Minimum]:
    override transparent inline def valid: false | Null | true = ${ implDouble[Value, Minimum] }

  private class CompileTimeCheckChar[Value <: Char, Minimum <: Char] extends CompileTimeCheck[Value AtLeast Minimum]:
    override transparent inline def valid: false | Null | true = ${ implChar[Value, Minimum] }

  private class CompileTimeCheckInt[Value <: Int, Minimum <: Int] extends CompileTimeCheck[Value AtLeast Minimum]:
    override transparent inline def valid: false | Null | true = ${ implInt[Value, Minimum] }

  private class CompileTimeCheckString[Value <: String, Minimum <: String] extends CompileTimeCheck[Value AtLeast Minimum]:
    override transparent inline def valid: false | Null | true = ${ implString[Value, Minimum] }

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

