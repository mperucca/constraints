import constraints.{CompileTimeComputation, RuntimeComputation}

import scala.quoted.*

sealed trait LessThan[A, B]

object LessThan:

  given runtimeComputation[A, B](using a: RuntimeComputation[A], b: RuntimeComputation[B])(using a.Result <:< Int, b.Result <:< Int): RuntimeComputation.Typed[LessThan[A, B], Boolean] =
    RuntimeComputation(a.result < b.result)

  transparent inline given compileTimeComputation[A, B](
    using inline a: CompileTimeComputation[A], inline b: CompileTimeComputation[B]
  )(using a.Result <:< Int, b.Result <:< Int): CompileTimeComputation.Typed[LessThan[A, B], Boolean] =
    inline a.result match
      case null => CompileTimeComputation.Unknown
      case l: Int => continue[l.type, B]

  // intermediary inline method needed since nested inline match doesn't reduce as intended
  transparent inline def continue[L <: Int, B](
    using inline b: CompileTimeComputation[B]
  )(using b.Result <:< Int): CompileTimeComputation.Typed[Any, Boolean] =
    inline b.result match
      case null => CompileTimeComputation.Unknown
      case h: Int => CompileTimeComputationImpl[L, h.type]

  class CompileTimeComputationImpl[A <: Int, B <: Int] extends CompileTimeComputation[Any]:
    override type Result = Boolean
    override transparent inline def result: Null | Boolean = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Null | Boolean] =
    CompileTimeComputation.fromRuntimeCheckOnTuple[(A, B), Boolean] { case (a, b) =>
      runtimeComputation[a.type, b.type]
    }

