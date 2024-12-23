import constraints.*
import constraints.compile.{Builtin, Inlinable}

import scala.annotation.tailrec
import scala.quoted.*

type Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): Compute.Predicate[Unique[I]] =
    Compute {
      val soFar = collection.mutable.Set.empty[A]
      valueOf[I].toIterable.forall(soFar.add)
    }

  given compileTimeCheckGroup[T <: Tuple: Builtin]: Inlinable[Unique[T]] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${implTuple[T]}

  given compileTimeCheckString[S <: String]: Inlinable[Unique[S]] with
    override type Result = Boolean
    override transparent inline def reduce: Option[Boolean] = ${ implString[S] }

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[Option[Boolean]] = impl

  private def implString[S <: String: Type](using Quotes): Expr[Option[Boolean]] = impl

  private def impl[I: Type, A](using Iterate[I, A], Quotes): Expr[Option[Boolean]] =
    Inlinable.fromComputationPostponingExtractableCheck((i: I) => summon[Compute[Unique[i.type]]])