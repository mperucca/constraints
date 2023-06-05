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

  trait Impl[R] extends Inlinable[Any]:
    override type Result = R

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Inlinable[E] { type Result <: R }

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
  given unknown: Inlinable.Typed[Null, Null] =
    Unknown

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
    inlineOption(FromType.builtin[E].extract)

  /**
   * Utility method for [[Inlinable]] implementations that evaluate a computation by attempting to
   * extract a constant value from type [[E]] and then calling the provided [[Computable]] with the value
   *
   * @param computable a function returning a [[Computable]] when given the value extracted from [[E]]
   * @param Quotes      performs operations in macro contexts
   * @tparam E the expression being computed
   * @tparam R the result type of the expression
   * @return an expression that either contains the literal result or null
   */
  def fromComputablePostponingExtractableCheck[E: Type, R: Type](
    computable: E => Computable.Typed[Nothing, R]
  )(using Quotes): Expr[Option[R]] =
    given Builtin[E] = Builtin.evidenceOrAbort
    given Builtin[R] = Builtin.evidenceOrAbort
    fromComputable(computable)

  def fromComputable[E: Type, R: Type: Literable: Refinable](
    computable: E => Computable.Typed[Nothing, R]
  )(using Quotes)(using extractable: FromType[E]): Expr[Option[R]] =
    inlineOption(extractable.extract.map(computable).map(_.compute))

  private def inlineOption[V: Type](possibleValue: Option[V])(using Quotes)(using literable: Literable[V], refinable: Refinable[V]): Expr[Option[V]] =
    possibleValue match
      case None => '{ None }
      case Some(value) =>
        val expr = literable.toLiteral(value)
        val tpe = refinable.refine(value)
        tpe.asType match
          case '[e] =>
            val casted = '{ $expr.asInstanceOf[e] }.asExprOf[e]
            '{ Some[e]($casted) }.asExprOf[Option[V]]
