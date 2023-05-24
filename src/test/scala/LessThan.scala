import constraints.{CompileTimeComputation, RuntimeComputation}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given runtimeComputation[A, B](
    using a: RuntimeComputation.Typed[A, Int], b: RuntimeComputation.Typed[B, Int]
  ): RuntimeComputation.Predicate[LessThan[A, B]] =
    RuntimeComputation(a.result < b.result)

  transparent inline given compileTimeComputation[A, B](
    using inline a: CompileTimeComputation.Typed[A, Int], inline b: CompileTimeComputation.Typed[B, Int]
  ): CompileTimeComputation.Predicate[LessThan[A, B]] =
    inline a.result match
      case null => CompileTimeComputation.Unknown
      case l: Int => continue[l.type, B]

  // intermediary inline method needed since nested inline match doesn't reduce as intended
  transparent inline def continue[L <: Int, B](
    using inline b: CompileTimeComputation.Typed[B, Int]
  ): CompileTimeComputation.Predicate[Any] =
    inline b.result match
      case null => CompileTimeComputation.Unknown
      case h: Int => CompileTimeComputationImpl[L, h.type]

  class CompileTimeComputationImpl[A <: Int, B <: Int] extends CompileTimeComputation[Any]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Boolean | Null] =
    CompileTimeComputation.fromRuntimeOnTuple[(A, B), Boolean] { case (a, b) =>
      runtimeComputation[a.type, b.type]
    }

