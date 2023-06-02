package constraints

import scala.quoted.*

/**
 * Type class for computing [[E]] at compile time through inlining.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 */
trait Inliner[-E]:

  /**
   * The result type of the computation
   */
  type Result

  /**
   * Inlines the result if it can be computed; otherwise, inlines null
   * @return
   */
  inline def reduce: Result | Null

object Inliner:

  trait Impl[R] extends Inliner[Any]:
    override type Result = R

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Inliner[E] { type Result <: R }

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
  transparent inline given unknown: Inliner.Typed[Null, Null] =
    Unknown

  /**
   * Used when a compile time computation cannot reduce to a definitive result
   * @note Will never be return so typed a [[Nothing]] for covariance reasons
   */
  object Unknown extends Inliner.Impl[Nothing]:
    /**
     * Hardcoded to always return null
     *  @return null
     */
    override inline def reduce: Null = null

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  transparent inline given literal[R <: Singleton]: Inliner.Typed[R, R] =
    value[R]

  /**
   * Type class instance for leaf types
   *
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  given value[R]: Inliner[R] with

    /**
     * The result type that a value might be extracted from
     */
    override type Result = R

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *
     * @return the extracted value or null
     */
    override transparent inline def reduce: Result | Null = ${ impl[R] }

  /**
   * Used when it is known what type to attempt extracting the value from
   * @tparam R the result to attempt value extraction from
   * @note similar to [[Value]] but with the expression typed as [[Any]] for contravariance
   */
  class Constant[R] extends Inliner.Impl[R]:

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *
     * @return the extracted value or null
     */
    override transparent inline def reduce: Result | Null = ${ impl[R] }

  private def impl[E: Type](using Quotes): Expr[E | Null] =
    given Extractable[E] = Extractable.evidenceOrAbort
    Extractable.extract[E] match
      case None => '{null}
      case Some(value) => Extractable.toExpr(value)

  /**
   * Utility method for [[Inliner]] implementations that evaluate a computation by attempting to
   * extract a constant value from type [[E]] and then calling the provided [[Computation]] with the value
   *
   * @param computation a function returning a [[Computation]] when given the value extracted from [[E]]
   * @param Quotes             performs operations in macro contexts
   * @tparam E the expression being computed
   * @tparam R the result type of the expression
   * @return an expression that either contains the literal result or null
   */
  def fromComputationPostponingExtractableCheck[E: Type, R: Type](
    computation: E => Computation.Typed[Nothing, R]
  )(using Quotes): Expr[R | Null] =
    given Extractable[E] = Extractable.evidenceOrAbort
    given Extractable[R] = Extractable.evidenceOrAbort
    fromComputation(computation)

  def fromComputation[E: Type: Extractable, R: Type: Extractable](
    computation: E => Computation.Typed[Nothing, R]
  )(using Quotes): Expr[R | Null] =
    Extractable.extract[E].map(computation) match
      case None => '{ null }
      case Some(computation) =>
        Extractable.toExpr[R](computation.compute)