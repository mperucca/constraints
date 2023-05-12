package constraints

import scala.quoted.*

trait CompileTimeComputation[-E]:

  type Result

  inline def result: Null | Result

object CompileTimeComputation:

  type Typed[-E, +R] = CompileTimeComputation[E] { type Result <: R }

  type Predicate[-E] = Typed[E, Boolean]

  transparent inline given unknown: CompileTimeComputation.Typed[Null, Null] =
    Unknown

  object Unknown extends CompileTimeComputation[Any]:
    override type Result = Nothing
    override inline def result: Null = null

  // necessary for type class inference to not widen
  transparent inline given literal[R <: Extractable & Singleton]: CompileTimeComputation.Typed[R, R] =
    value[R]

  transparent inline given value[R <: Extractable]: CompileTimeComputation.Typed[R, R] =
    Value[R]

  class Value[R <: Extractable] extends CompileTimeComputation[R]:
    override type Result = R
    override transparent inline def result: Null | Result = ${ impl[R] }

  class Constant[R <: Extractable] extends CompileTimeComputation[Any]:
    override type Result = R
    override transparent inline def result: Null | Result = ${ impl[R] }

  private def impl[E <: Extractable: Type](using Quotes): Expr[Null | E] =
    Extractable.extract[E] match
      case None => '{null}
      case Some(value) => Extractable.toExpr(value)

  def fromRuntimeComputationOnConstant[E <: Extractable: Type, R <: Extractable](
    runtimeComputation: E => RuntimeComputation.Typed[Nothing, R]
  )(using Quotes): Expr[Null | R] =
    fromRuntimeComputation(Extractable.extract[E].map(runtimeComputation))

  def fromRuntimeCheckOnTuple[T <: Tuple : Type, R <: Extractable](
    runtimeComputation: T => RuntimeComputation.Typed[Nothing, R]
  )(using Quotes, Tuple.Union[T] <:< Extractable): Expr[Null | R] =
    fromRuntimeComputation(
      Extractable.extract[Group.FromTuple[T]].map(v => runtimeComputation(v.toTuple.asInstanceOf[T]))
    )

  def fromRuntimeComputation[R <: Extractable](
    possibleRuntimeComputation: Option[RuntimeComputation.Typed[Nothing, R]]
  )(using Quotes): Expr[Null | R] =
    possibleRuntimeComputation match
      case None => '{ null }
      case Some(runtimeComputation) => Extractable.toExpr(runtimeComputation.result)

  /**
   * Type class instance of [[CompileTimeComputation]] for [[Not]]
   *
   * @param c the [[CompileTimeComputation]] instance to negate
   * @tparam C the constraint to negate
   * @return a [[CompileTimeComputation]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given[C](
    using inline c: CompileTimeComputation.Predicate[C]
  ): CompileTimeComputation.Predicate[Not[C]] =
    inline c.result match
      case false => CompileTimeComputation.Constant[true]
      case null => CompileTimeComputation.Unknown
      case true => CompileTimeComputation.Constant[false]

  /**
   * Type class instance of [[CompileTimeComputation]] for [[and]]
   *
   * @param a the first [[CompileTimeComputation]] of the conjunction
   * @param b the second [[CompileTimeComputation]] of the conjunction
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[CompileTimeComputation]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
  transparent inline given[A, B](
    using inline a: CompileTimeComputation.Predicate[A], inline b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A and B] =
    inline a.result match
      case false => CompileTimeComputation.Constant[false]
      case null => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null | true => CompileTimeComputation.Unknown
      case true => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]

  /**
   * Type class instance of [[CompileTimeComputation]] for [[or]]
   *
   * @param a the first [[CompileTimeComputation]] of the inclusive disjunction
   * @param b the second [[CompileTimeComputation]] of the inclusive disjunction
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[CompileTimeComputation]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
  transparent inline given[A, B](
    using inline a: CompileTimeComputation.Predicate[A], inline b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A or B] =
    inline a.result match
      case false => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case null => inline b.result match
        case false | null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case true => CompileTimeComputation.Constant[true]

  /**
   * Type class instance of [[CompileTimeComputation]] for [[xor]]
   *
   * @param a the first [[CompileTimeComputation]] of the exclusive disjunction
   * @param b the second [[CompileTimeComputation]] of the exclusive disjunction
   * @tparam A the first constraint of the exclusive disjunction
   * @tparam B the second constraint of the exclusive disjunction
   * @return a [[CompileTimeComputation]] for the exclusive disjunction of [[A]] xor [[B]]
   *         either being unknown results in unknown
   *         both being known results in false if they are the same and true if they are different
   */
  transparent inline given[A, B](
    using inline a: CompileTimeComputation.Predicate[A], inline b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A xor B] =
    inline a.result match
      case false => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case null => CompileTimeComputation.Unknown
      case true => inline b.result match
        case false => CompileTimeComputation.Constant[true]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[false]

