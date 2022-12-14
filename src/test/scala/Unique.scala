import constraints.{CompileTimeCheck, Iterate, Witness, RuntimeCheck}

import scala.annotation.tailrec
import scala.quoted.*

trait Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): RuntimeCheck[Unique[I]] = RuntimeCheck {
    val soFar = collection.mutable.Set.empty[A]
    valueOf[I].toIterable.forall(soFar.add)
  }

  transparent inline given tuple[T <: Tuple]: CompileTimeCheck[Unique[T]] =
    UniqueCompileTimeCheck[T]

  private class UniqueCompileTimeCheck[T <: Tuple] extends CompileTimeCheck[Unique[T]]:
    override transparent inline def valid: false | Null | true = ${implTuple[T]}

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnPossibleConstantTuple((t: T) => summon[RuntimeCheck[Unique[t.type]]])

  transparent inline given string[S <: String]: CompileTimeCheck[Unique[S]] =
    UniqueCompileTimeCheckString[S]

  private class UniqueCompileTimeCheckString[S <: String] extends CompileTimeCheck[Unique[S]]:
    override transparent inline def valid: false | Null | true = ${implString[S]}

  private def implString[S <: String: Type](using Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnPossibleConstant((s: S) => summon[RuntimeCheck[Unique[s.type]]])