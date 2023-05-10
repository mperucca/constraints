import constraints.{CompileTimeCheck, CompileTimeComputation, Extractable, Group, Guarantee, Iterate, RuntimeCheck}

import scala.annotation.tailrec
import scala.quoted.*

trait Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): RuntimeCheck[Unique[I]] = RuntimeCheck {
    val soFar = collection.mutable.Set.empty[A]
    valueOf[I].toIterable.forall(soFar.add)
  }

  transparent inline given compileTimeCheckGroup[T <: Group]: CompileTimeComputation.Typed[Unique[T], Boolean] =
    CompileTimeCheckGroup[T]

  transparent inline given compileTimeCheckString[S <: String]: CompileTimeComputation.Typed[Unique[S], Boolean] =
    CompileTimeCheckString[S]

  private class CompileTimeCheckGroup[T <: Group] extends CompileTimeComputation[Unique[T]]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${implTuple[T]}

  private class CompileTimeCheckString[S <: String] extends CompileTimeComputation[Unique[S]]:
    override type Result = Boolean
    override transparent inline def result: false | Null | true = ${ implString[S] }

  private def implTuple[T <: Group: Type](using Quotes): Expr[false | Null | true] = impl

  private def implString[S <: String: Type](using Quotes): Expr[false | Null | true] = impl

  private def impl[I <: Extractable: Type, A](using Iterate[I, A], Quotes): Expr[false | Null | true] =
    CompileTimeCheck.fromRuntimeCheckOnConstant((i: I) => summon[RuntimeCheck[Unique[i.type]]])