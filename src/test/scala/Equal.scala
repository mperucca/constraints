import constraints.*

import quoted.*
import scala.annotation.targetName

sealed trait Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given runtimeCheck[A: ValueOf, B: ValueOf]: RuntimeComputation.Predicate[A === B] =
    RuntimeComputation(valueOf[A] == valueOf[B])

  transparent inline given compileTimeCheckEqual[
    A <: Extractable,
    B <: Extractable
  ]: CompileTimeComputation.Predicate[A === B] =
    CompileTimeCheckEqual[A, B]

  private class CompileTimeCheckEqual[
    A <: Extractable,
    B <: Extractable
  ] extends CompileTimeComputation[A === B]:
    override type Result = Boolean
    override transparent inline def result: Null | Boolean = ${impl[A, B]}

  private def impl[
    A <: Extractable: Type,
    B <: Extractable: Type
  ](using Quotes): Expr[Null | Boolean] =
    CompileTimeComputation.fromRuntimeCheckOnTuple[(A, B), Boolean] {
      case (a, b) => runtimeCheck[a.type, b.type]
    }
