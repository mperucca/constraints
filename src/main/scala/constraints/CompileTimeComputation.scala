package constraints

import scala.quoted.*

/**
 * Type class for computing [[E]] at compile time through inlining.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 */
trait CompileTimeComputation[-E]:

  /**
   * The result type of the computation
   */
  type Result

  /**
   * Inlines the result if it can be computed; otherwise, inlines null
   * @return
   */
  inline def result: Result | Null

object CompileTimeComputation:

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = CompileTimeComputation[E] { type Result <: R }

  /**
   * Type alias for computations returning [[Boolean]]s
   *
   * @tparam E the expression to compute
   */
  type Predicate[-E] = Typed[E, Boolean]

  /**
   * Type class instance of a compile time computation result being unknown, represented by null
   * @return the type class instance for unknown computation results
   */
  transparent inline given unknown: CompileTimeComputation.Typed[Null, Null] =
    Unknown

  /**
   * Used when a compile time computation cannot reduce to a definitive result
   */
  object Unknown extends CompileTimeComputation[Any]:
    /**
     * Will never be return so typed a [[Nothing]] for covariance reasons
     */
    override type Result = Nothing
    /**
     * Hardcoded to always return null
     *  @return null
     */
    override inline def result: Null = null

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  transparent inline given literal[R <: Extractable & Singleton]: CompileTimeComputation.Typed[R, R] =
    value[R]

  /**
   * Type class instance for leaf types
   *
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  transparent inline given value[R <: Extractable]: CompileTimeComputation.Typed[R, R] =
    Value[R]

  /**
   * Used when attempting to extract a value from a type
   * @tparam R the result to attempt value extraction from
   */
  class Value[R <: Extractable] extends CompileTimeComputation[R]:
    /**
     * The result type that a value might be extracted from
     */
    override type Result = R

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *  @return the extracted value or null
     */
    override transparent inline def result: Result | Null = ${ impl[R] }

  /**
   * Used when it is known what type to attempt extracting the value from
   * @tparam R the result to attempt value extraction from
   * @note similar to [[Value]] but with the expression typed as [[Any]] for contravariance
   */
  class Constant[R <: Extractable] extends CompileTimeComputation[Any]:
    /**
     * The result type that a value might be extracted from
     */
    override type Result = R

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *
     * @return the extracted value or null
     */
    override transparent inline def result: Result | Null = ${ impl[R] }

  private def impl[E <: Extractable: Type](using Quotes): Expr[E | Null] =
    Extractable.extract[E] match
      case None => '{null}
      case Some(value) => Extractable.toExpr(value)

  /**
   * Utility method for [[CompileTimeComputation]] implementations that evaluate a computation by attempting to
   * extract a constant value from type [[E]] and then calling the provided [[RuntimeComputation]] with the value
   *
   * @param runtimeComputation a function returning a [[RuntimeComputation]] when given the value extracted from [[E]]
   * @param Quotes performs operations in macro contexts
   * @tparam E the expression being computed
   * @tparam R the result type of the expression
   * @return an expression that either contains the literal result or null
   */
  def fromRuntimeComputationOnConstant[E <: Extractable: Type, R <: Extractable](
    runtimeComputation: E => RuntimeComputation.Typed[Nothing, R]
  )(using Quotes): Expr[R | Null] =
    fromRuntimeComputation(Extractable.extract[E].map(runtimeComputation))

  /**
   * Utility method for [[CompileTimeComputation]] implementations that evaluates an expression type by attempting to
   * extract values from [[Tuple]] type [[T]] and then calling the provided [[RuntimeComputation]] with the values
   *
   * @param runtimeComputation a function returning a [[RuntimeComputation]] when given the values extracted from [[T]]
   * @param Quotes             performs operations in macro contexts
   * @tparam T the tuple to extract values from
   * @tparam R the result type of the expression
   * @return an expression that either contains the literal result or null
   */
  def fromRuntimeCheckOnTuple[T <: Tuple : Type, R <: Extractable](
    runtimeComputation: T => RuntimeComputation.Typed[Nothing, R]
  )(using Quotes, Tuple.Union[T] <:< Extractable): Expr[R | Null] =
    fromRuntimeComputation(
      Extractable.extract[Group.FromTuple[T]].map(v => runtimeComputation(v.toTuple.asInstanceOf[T]))
    )

  def fromRuntimeComputation[R <: Extractable](
    possibleRuntimeComputation: Option[RuntimeComputation.Typed[Nothing, R]]
  )(using Quotes): Expr[R | Null] =
    possibleRuntimeComputation match
      case None => '{ null }
      case Some(runtimeComputation) => Extractable.toExpr(runtimeComputation.result)
