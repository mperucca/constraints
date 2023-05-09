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
      case null => CompileTimeComputation.Unknown[LessThan[A, B], Boolean]
      case l: Int =>
        inline b.result match
          case null => CompileTimeComputation.Unknown[LessThan[A, B], Boolean]
          case h: Int => CompileTimeComputationImpl[l.type, h.type, A, B]

  class CompileTimeComputationImpl[A <: Int, B <: Int, Y, Z] extends CompileTimeComputation[LessThan[Y, Z]]:
    override type Result = Boolean
    override transparent inline def result: Null | Boolean = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Null | Boolean] =
    scala.quoted.quotes.reflect.report.info(scala.quoted.quotes.reflect.TypeRepr.of[B].show)
    CompileTimeComputation.fromRuntimeCheckOnTuple[(A, B), Boolean] { case (a, b) =>
      runtimeComputation[a.type, b.type]
    }

