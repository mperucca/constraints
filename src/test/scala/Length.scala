import constraints.*
import constraints.RuntimeComputation.Typed

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length:

  given runtimeComputation[S](
    using c: RuntimeComputation.Typed[S, String]
  ): RuntimeComputation.Typed[Length[S], Int] =
    RuntimeComputation(c.result.length)

  transparent inline given compileTimeComputation[S](
    using c: CompileTimeComputation.Typed[S, String]
  ): CompileTimeComputation.Typed[Length[S], Int] =
    inline c.result match
      case null => CompileTimeComputation.Unknown
      case s: String => CompileTimeComputationImpl[s.type]

  class CompileTimeComputationImpl[S <: String] extends CompileTimeComputation.Impl[Int]:
    override transparent inline def result: Int | Null = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Int | Null] =
    CompileTimeComputation.fromRuntime((s: S) => runtimeComputation[s.type])