import constraints.*

import quoted.*
import scala.annotation.targetName

sealed trait Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given runtimeCheck[A: ValueOf, B: ValueOf]: RuntimeCheck[A === B] = RuntimeCheck(valueOf[A] == valueOf[B])

  transparent inline given compileTimeCheckEqual[A, B]: CompileTimeCheck[A === B] = CompileTimeCheckEqual[A, B]

  private class CompileTimeCheckEqual[A, B] extends CompileTimeCheck[A === B]:
    override transparent inline def valid: false | Null | true = ${impl[A, B]}

  private def impl[A: Type, B: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnConstant[(A, B)] {
      case (a, b) => runtimeCheck[a.type, b.type]
    }
