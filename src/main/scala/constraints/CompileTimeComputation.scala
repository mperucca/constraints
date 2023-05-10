package constraints

import scala.quoted.*

trait CompileTimeComputation[-E]:

  type Result

  inline def result: Null | Result

object CompileTimeComputation:

  type Typed[-E, +R] = CompileTimeComputation[E] { type Result <: R }

  transparent inline given unknown: CompileTimeComputation.Typed[Null, Null] =
    Unknown

  object Unknown extends CompileTimeComputation[Any]:
    override type Result = Nothing
    override inline def result: Null = null

  transparent inline given constantSingleton[R <: Extractable & Singleton]: CompileTimeComputation.Typed[R, R] =
    Constant[R]

  transparent inline given constant[R <: Extractable]: CompileTimeComputation.Typed[R, R] =
    Constant[R]

  class Constant[R <: Extractable] extends CompileTimeComputation[R]:
    override type Result = R
    override transparent inline def result: Null | Result = ${ impl[R] }

  private def impl[E <: Extractable: Type](using Quotes): Expr[Null | E] =
    Extractable.extract[E] match
      case None => '{null}
      case Some(value) => Extractable.toExpr(value)

  def fromRuntimeComputationOnConstant[E <: Extractable: Type, R <: Extractable](runtimeComputation: (e: E) => RuntimeComputation.Typed[_, R])(using Quotes): Expr[Null | R] =
    fromRuntimeComputation(Extractable.extract[E].map(runtimeComputation))

  def fromRuntimeCheckOnTuple[T <: Tuple : Type, R <: Extractable](runtimeComputation: (v: T) => RuntimeComputation.Typed[_, R])(using Quotes, Tuple.Union[T] <:< Extractable): Expr[Null | R] =
    fromRuntimeComputation(Extractable.extract[Group.FromTuple[T]].map(v => runtimeComputation(v.toTuple.asInstanceOf[T])))

  def fromRuntimeComputation[R <: Extractable](possibleRuntimeComputation: Option[RuntimeComputation.Typed[_, R]])(using Quotes): Expr[Null | R] =
    possibleRuntimeComputation match
      case None => '{ null }
      case Some(runtimeComputation) => Extractable.toExpr(runtimeComputation.result)
