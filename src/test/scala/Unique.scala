import constraints.{CompileTimeCheck, Iterate, Witness, RuntimeCheck}

import scala.annotation.tailrec
import scala.quoted.*

trait Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): RuntimeCheck[Unique[I]] = RuntimeCheck {
    val soFar = collection.mutable.Set.empty[A]
    valueOf[I].toIterable.forall(soFar.add)
  }

  transparent inline given compileTimeCheckTuple[T <: Tuple]: CompileTimeCheck[Unique[T]] =
    CompileTimeCheckTuple[T]

  transparent inline given compileTimeCheckString[S <: String]: CompileTimeCheck[Unique[S]] =
    CompileTimeCheckString[S]

  private class CompileTimeCheckTuple[T <: Tuple] extends CompileTimeCheck[Unique[T]]:
    override transparent inline def valid: false | Null | true = ${implTuple[T]}

  private class CompileTimeCheckString[S <: String] extends CompileTimeCheck[Unique[S]]:
    override transparent inline def valid: false | Null | true = ${ implString[S] }

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[false | Null | true] = impl

  private def implString[S <: String: Type](using Quotes): Expr[false | Null | true] = impl

  private def impl[I: Type, A](using Iterate[I, A], Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnPossibleConstant((i: I) => summon[RuntimeCheck[Unique[i.type]]])