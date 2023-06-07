package constraints

import scala.quoted.*

/**
 * Type class for computing [[E]] at compile time through inlining.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 */
trait Inlinable[-E]:

  /**
   * The result type of the computation
   */
  type Result

  /**
   * Inlines the result if it can be computed; otherwise, inlines null
   * @return
   */
  inline def reduce: Option[Result]

  transparent inline def inlined: Result =
    inline reduce match
      case Some(result) => result
      case None => compiletime.error("could not inline because the value is unknown")

object Inlinable:

  transparent inline def reduce[A](using inlinable: Inlinable[A]): Option[inlinable.Result] = inlinable.reduce

  trait Impl[R] extends Inlinable[Any]:
    override type Result = R

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Inlinable[E] { type Result <: R }

  type From[-E] = [R] =>> Typed[E, R]

  type To[+R] = [E] =>> Typed[E, R]

  /**
   * Used when a compile time computation cannot reduce to a definitive result
   * @note Will never be return so typed a [[Nothing]] for covariance reasons
   */
  object Unknown extends Inlinable.Impl[Nothing]:
    /**
     * Hardcoded to always return null
     *  @return null
     */
    override inline def reduce: None.type = None

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  transparent inline given builtinSingleton[R <: Singleton: Builtin]: Inlinable.Typed[R, R] =
    builtin[R]

  /**
   * Type class instance for leaf types
   *
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  given builtin[R: Builtin]: Inlinable[R] with

    /**
     * The result type that a value might be extracted from
     */
    override type Result = R

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *
     * @return the extracted value or null
     */
    override transparent inline def reduce: Option[Result] = ${ impl[R] }

  /**
   * Used when it is known what type to attempt extracting the value from
   * @tparam R the result to attempt value extraction from
   * @note similar to [[Value]] but with the expression typed as [[Any]] for contravariance
   */
  class Constant[R: Builtin] extends Inlinable.Impl[R]:

    /**
     * Runs the value extraction macro to attempt to extract a value from [[R]]
     *
     * @return the extracted value or null
     */
    override transparent inline def reduce: Option[Result] = ${ impl[R] }

  private def impl[E: Type](using Quotes): Expr[Option[E]] =
    given Builtin[E] = Builtin.evidenceOrAbort
    import Builtin.toExpr
    inlineOption(FromType[E])

  /**
   * Utility method for [[Inlinable]] implementations that evaluate a computation by attempting to
   * extract a constant value from type [[E]] and then calling the provided [[Compute]] with the value
   *
   * @param computable a function returning a [[Compute]] when given the value extracted from [[E]]
   * @param Quotes     performs operations in macro contexts
   * @tparam E the expression being computed
   * @tparam R the result type of the expression
   * @return an expression that either contains the literal result or null
   */
  def fromComputationPostponingExtractableCheck[E: Type, R: Type](
    computable: E => Compute.Typed[Nothing, R]
  )(using Quotes): Expr[Option[R]] =
    given Builtin[E] = Builtin.evidenceOrAbort
    given Builtin[R] = Builtin.evidenceOrAbort
    import Builtin.toExpr
    fromComputation(computable)

  def fromComputation[E: Type: FromType, R: Type: ToExpr: ToType](
    computable: E => Compute.Typed[Nothing, R]
  )(using Quotes): Expr[Option[R]] =
    inlineOption(FromType[E].map(computable).map(_.compute))

  private def inlineOption[V: Type: ToExpr: ToType](possibleValue: Option[V])(using Quotes): Expr[Option[V]] =
    possibleValue match
      case None => '{ None }
      case Some(value) =>
        val expr = Expr(value)
        val tpe = ToType(value)
        tpe match
          case '[e] =>
            val casted = '{ $expr.asInstanceOf[e] }.asExprOf[e]
            '{ Some[e]($casted) }.asExprOf[Option[V]]
