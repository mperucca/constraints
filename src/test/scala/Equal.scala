import constraints.*

import quoted.*
import scala.annotation.targetName

type Equal[A, B]
@targetName("Equal")
type ===[A, B] = Equal[A, B]

object Equal:

  given runtimeComputation[A, B](
    using a: RuntimeComputation.Typed[A, Any], b: RuntimeComputation.Typed[B, Any]
  ): RuntimeComputation.Predicate[A === B] =
    RuntimeComputation(a.result == b.result)

  transparent inline given compileTimeComputation[A, B](
    using a: CompileTimeComputation.Typed[A, Any], b: CompileTimeComputation.Typed[B, Any]
  ): CompileTimeComputation.Predicate[A === B] =
    inline a.result match
      case null => CompileTimeComputation.Unknown
      case r1: Primitive => continue[r1.type, B] // use literal type for primitive
      case r1: Tuple => continue[a.Result, B] // literal types don't reduce for tuples

  // intermediary inline method needed since nested inline match doesn't reduce as intended
  transparent inline def continue[A, B](
    using b: CompileTimeComputation.Typed[B, Any]
  ): CompileTimeComputation.Predicate[Any] =
    inline b.result match
      case null => CompileTimeComputation.Unknown
      case r2: Primitive => CompileTimeComputationImpl[A, r2.type]
      case r2: Tuple => CompileTimeComputationImpl[A, b.Result]

  class CompileTimeComputationImpl[A, B] extends CompileTimeComputation.Impl[Boolean]:
    override transparent inline def result: Boolean | Null = ${ impl[A, B] }

  private def impl[A : Type, B : Type](using Quotes): Expr[Boolean | Null] =
    CompileTimeComputation.fromRuntime[(A, B), Boolean] { case (a, b) =>
      runtimeComputation[a.type, b.type]
    }
