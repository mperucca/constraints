import constraints.*

import quoted.*
import scala.annotation.targetName

sealed trait Unequal[A, B]
@targetName("Unequal")
type !==[A, B] = Unequal[A, B]

object Unequal:

  given runtimeCheck[A: ValueOf, B: ValueOf]: RuntimeCheck[A !== B] = RuntimeCheck(valueOf[A] != valueOf[B])

  transparent inline given proveUnequal[A, B]: CompileTimeCheck[A !== B] = ProveUnequal[A, B]
  transparent inline given proveUnequalTuple[A <: Tuple, B <: Tuple]: CompileTimeCheck[A !== B] = ProveUnequalTuple[A, B]

  private class ProveUnequal[A, B] extends CompileTimeCheck[A !== B]:
    override transparent inline def valid: false | Null | true = ${impl[A, B]}

  private class ProveUnequalTuple[A <: Tuple, B <: Tuple] extends CompileTimeCheck[A !== B]:
    override transparent inline def valid: false | Null | true = ${ implTuple[A, B] }

  private def impl[A: Type, B: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnPossibleConstantTuple[(A, B)] {
      case (a, b) => runtimeCheck[a.type, b.type]
    }

  private def implTuple[A <: Tuple: Type, B <: Tuple: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnPossibleConstantTuple[(A, B)] {
      case (a, b) => runtimeCheck[a.type, b.type]
    }
