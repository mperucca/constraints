import constraints.*

import scala.annotation.tailrec
import scala.quoted.*

type Unique[A]

object Unique:

  given runtimeCheck[I: ValueOf, A](using Iterate[I, A]): Computable.Predicate[Unique[I]] =
    Computable {
      val soFar = collection.mutable.Set.empty[A]
      valueOf[I].toIterable.forall(soFar.add)
    }

  given compileTimeCheckGroup[T <: Tuple: Extractable]: Inlinable[Unique[T]] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${implTuple[T]}

  given compileTimeCheckString[S <: String]: Inlinable[Unique[S]] with
    override type Result = Boolean
    override transparent inline def reduce: Boolean | Null = ${ implString[S] }

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[Boolean | Null] = impl

  private def implString[S <: String: Type](using Quotes): Expr[Boolean | Null] = impl

  private def impl[I: Type, A](using Iterate[I, A], Quotes): Expr[Boolean | Null] =
    Inlinable.fromComputablePostponingExtractableCheck((i: I) => summon[Computable[Unique[i.type]]])