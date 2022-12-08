import constraints.{CompileTimeCheck, Iterate, Proof, RuntimeCheck}

import scala.annotation.tailrec
import scala.quoted.*

trait Unique[A]

object Unique:

  given [I: ValueOf, A](using Iterate[I, A]): RuntimeCheck[Unique[I]] = RuntimeCheck {
    val soFar = collection.mutable.Set.empty[A]
    valueOf[I].toIterable.forall(soFar.add)
  }

  transparent inline given tuple[T <: Tuple]: CompileTimeCheck[Unique[T]] =
    UniqueCompileTimeCheck[T]

  private class UniqueCompileTimeCheck[T <: Tuple] extends CompileTimeCheck[Unique[T]]:
    override transparent inline def valid: Boolean | Null = ${implTuple[T]}

  private def implTuple[T <: Tuple: Type](using Quotes): Expr[Boolean | Null] =
    Type.valueOfTuple[T].fold('{null}) { t =>
      Expr(summon[RuntimeCheck[Unique[t.type]]].succeeds)
    }

  transparent inline given string[S <: String]: CompileTimeCheck[Unique[S]] =
    UniqueCompileTimeCheckString[S]

  private class UniqueCompileTimeCheckString[S <: String] extends CompileTimeCheck[Unique[S]]:
    override transparent inline def valid: Boolean | Null = ${ implString[S] }

  private def implString[S <: String: Type](using Quotes): Expr[Boolean | Null] =
    Type.valueOfConstant[S].fold('{null}) { s =>
      Expr(summon[RuntimeCheck[Unique[s.type]]].succeeds)
    }