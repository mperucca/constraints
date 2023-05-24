import constraints.*

import scala.annotation.tailrec
import scala.quoted.*

type Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): RuntimeComputation.Predicate[Unique[I]] =
    RuntimeComputation {
      val soFar = collection.mutable.Set.empty[A]
      valueOf[I].toIterable.forall(soFar.add)
    }

  transparent inline given compileTimeCheckGroup[T <: Tuple]: CompileTimeComputation.Predicate[Unique[T]] =
    CompileTimeCheckGroup[T]

  transparent inline given compileTimeCheckString[S <: String]: CompileTimeComputation.Predicate[Unique[S]] =
    CompileTimeCheckString[S]

  private class CompileTimeCheckGroup[T <: Tuple] extends CompileTimeComputation[Unique[T]]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${implTuple[T]}

  private class CompileTimeCheckString[S <: String] extends CompileTimeComputation[Unique[S]]:
    override type Result = Boolean
    override transparent inline def result: Boolean | Null = ${ implString[S] }

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[Boolean | Null] = impl

  private def implString[S <: String: Type](using Quotes): Expr[Boolean | Null] = impl

  private def impl[I: Type, A](using Iterate[I, A], Quotes): Expr[Boolean | Null] =
    CompileTimeComputation.fromRuntime((i: I) => summon[RuntimeComputation[Unique[i.type]]])