import constraints.{CompileTimeComputation, Extractable, Group, Guarantee, Iterate, RuntimeComputation}

import scala.annotation.tailrec
import scala.quoted.*

trait Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): RuntimeComputation.Typed[Unique[I], Boolean] = RuntimeComputation {
    val soFar = collection.mutable.Set.empty[A]
    valueOf[I].toIterable.forall(soFar.add)
  }

  transparent inline given compileTimeCheckGroup[T <: Group]: CompileTimeComputation.Typed[Unique[T], Boolean] =
    CompileTimeCheckGroup[T]

  transparent inline given compileTimeCheckString[S <: String]: CompileTimeComputation.Typed[Unique[S], Boolean] =
    CompileTimeCheckString[S]

  private class CompileTimeCheckGroup[T <: Group] extends CompileTimeComputation[Unique[T]]:
    override type Result = Boolean
    override transparent inline def result: Null | Boolean = ${implTuple[T]}

  private class CompileTimeCheckString[S <: String] extends CompileTimeComputation[Unique[S]]:
    override type Result = Boolean
    override transparent inline def result: Null | Boolean = ${ implString[S] }

  private def implTuple[T <: Group: Type](using Quotes): Expr[Null | Boolean] = impl

  private def implString[S <: String: Type](using Quotes): Expr[Null | Boolean] = impl

  private def impl[I <: Extractable: Type, A](using Iterate[I, A], Quotes): Expr[Null | Boolean] =
    CompileTimeComputation.fromRuntimeComputationOnConstant((i: I) => summon[RuntimeComputation[Unique[i.type]]])