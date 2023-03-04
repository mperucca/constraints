import constraints.{CompileTimeCheck, RuntimeCheck}

import scala.quoted.{Expr, Quotes, Type}

sealed trait AtLeast[Value, Minimum]

object AtLeast:

  given runtimeCheck[Value <: Orderable: ValueOf, Minimum <: Orderable: ValueOf, Orderable: Ordering]: RuntimeCheck[Value AtLeast Minimum] =
    RuntimeCheck(Ordering[Orderable].lteq(valueOf[Minimum], valueOf[Value]))

  transparent inline given [Value <: Double, Minimum <: Double]: CompileTimeCheck[Value AtLeast Minimum] =
    CompileTimeCheckAtLeast[Value, Minimum]

  private class CompileTimeCheckAtLeast[Value <: Double, Minimum <: Double] extends CompileTimeCheck[Value AtLeast Minimum]:
    override transparent inline def valid: false | Null | true = ${impl[Value, Minimum]}

  private def impl[Value <: Double: Type, Minimum <: Double: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnConstant[(Value, Minimum)] {
      case (value, minimum) => runtimeCheck[value.type, minimum.type, Double]
    }
