import constraints.*

import quoted.*
import scala.annotation.targetName

sealed trait Unequal[A, B]
@targetName("Unequal")
type !==[A, B] = Unequal[A, B]

object Unequal:

  given [A: ValueOf, B: ValueOf]: RuntimeCheck[A !== B] = RuntimeCheck(valueOf[A] != valueOf[B])

  transparent inline given [A, B]: CompileTimeCheck[A !== B] = ProveUnequal[A, B]
  transparent inline given proveUnequalTuple[A <: Tuple, B <: Tuple]: CompileTimeCheck[A !== B] = ProveUnequalTuple[A, B]

  private class ProveUnequal[A, B] extends CompileTimeCheck[A !== B]:
    override transparent inline def valid: Boolean | Null = ${impl[A, B]}

  private class ProveUnequalTuple[A <: Tuple, B <: Tuple] extends CompileTimeCheck[A !== B]:
    override transparent inline def valid: Boolean | Null = ${ implTuple[A, B] }

  private def impl[A: Type, B: Type](using Quotes): Expr[Boolean | Null] =
    implShared(Type.valueOfConstant[A], Type.valueOfConstant[B])

  private def implTuple[A <: Tuple: Type, B <: Tuple: Type](using Quotes): Expr[Boolean | Null] =
    implShared(Type.valueOfTuple[A], Type.valueOfTuple[B])

  private def implShared[A, B](valueOfA: => Option[A], valueOfB: => Option[B])(using Quotes): Expr[Boolean | Null] =
    val checked: Option[Expr[Boolean]] =
      for a <- valueOfA
          b <- valueOfB
      yield Expr[Boolean](summon[RuntimeCheck[a.type !== b.type]].succeeds)
    checked.getOrElse('{ null })
    