import constraints.*
import quoted.*

sealed trait Unequal[A, B]
type !==[A, B] = Unequal[A, B]

object Unequal :

  given[A: ValueOf, B: ValueOf]: RuntimeCheck[A !== B] = RuntimeCheck(valueOf[A] != valueOf[B])

  transparent inline given [A, B]: CompileTimeCheck[A !== B] = ProveUnequal[A, B]

  class ProveUnequal[A, B] extends CompileTimeCheck[A !== B]:
    override transparent inline def valid: Boolean | Null = ${impl[A, B]}

  def impl[A: Type, B: Type](using Quotes): Expr[Boolean | Null] =
    val checked: Option[Expr[Boolean]] =
      for a <- Type.valueOfConstant[A]
          b <- Type.valueOfConstant[B]
      yield Expr[Boolean](summon[RuntimeCheck[a.type !== b.type]].succeeds)
    checked.getOrElse('{null})
