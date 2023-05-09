import constraints.*
import scala.quoted.{Expr, Quotes, Type}

sealed trait Length[I]

object Length:

  given runtimeComputation[S](using c: RuntimeComputation[S])(using c.Result <:< String): RuntimeComputation.Typed[Length[S], Int] =
    RuntimeComputation(c.result.length)

  transparent inline given compileTimeComputation[S](using inline c: CompileTimeComputation[S])(using c.Result <:< String): CompileTimeComputation.Typed[Length[S], Int] =
    inline c.result match
      case null => CompileTimeComputation.Unknown[Length[S], Int]
      case s: String => CompileTimeComputationImpl[s.type, S]

  class CompileTimeComputationImpl[S <: String, L] extends CompileTimeComputation[Length[L]]:
    override type Result = Int
    override transparent inline def result: Null | Int = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Null | Int] =
    CompileTimeComputation.fromRuntimeComputationOnConstant((s: S) => runtimeComputation[s.type])