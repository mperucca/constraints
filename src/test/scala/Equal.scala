import constraints.*

import quoted.*
import scala.annotation.targetName

sealed trait Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given runtimeCheck[A: ValueOf, B: ValueOf]: RuntimeCheck[A === B] = RuntimeCheck(valueOf[A] == valueOf[B])

  transparent inline given compileTimeCheckEqual[A <: Extractable, B <: Extractable]: CompileTimeComputation.Typed[A === B, Boolean] = CompileTimeCheckEqual[A, B]

  private class CompileTimeCheckEqual[A <: Extractable, B <: Extractable] extends CompileTimeComputation[A === B]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${impl[A, B]}

  private def impl[A <: Extractable: Type, B <: Extractable: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnTuple[(A, B)] {
      case (a, b) => runtimeCheck[a.type, b.type]
    }
