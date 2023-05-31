import constraints.{CompileTimeComputation, RuntimeComputation}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given runtimeComputation[A, B](
    using a: RuntimeComputation.Typed[A, Int], b: RuntimeComputation.Typed[B, Int]
  ): RuntimeComputation.Predicate[LessThan[A, B]] =
    RuntimeComputation(a.result < b.result)

  transparent inline given compileTimeComputation[A, B](
    using a: CompileTimeComputation.Typed[A, Int], b: CompileTimeComputation.Typed[B, Int]
  ): CompileTimeComputation.Predicate[LessThan[A, B]] =
    inline a.result match
      case null => CompileTimeComputation.Unknown
      case l: Int =>
        inline b.result match
          case null => CompileTimeComputation.Unknown
          case h: Int => CompileTimeComputationImpl[l.type, h.type]

  class CompileTimeComputationImpl[A <: Int, B <: Int] extends CompileTimeComputation.Impl[Boolean]:
    override transparent inline def result: Boolean | Null = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Boolean | Null] =
    CompileTimeComputation.fromRuntime[(A, B), Boolean] { case (a, b) =>
      runtimeComputation[a.type, b.type]
    }

