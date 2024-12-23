package constraints.compile

import constraints.{Compute, Not, and, or, xor}

import scala.quoted.*

/**
 * Type class for computing [[E]] at compile time through inlining.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 * @tparam E the expression to inline
 */
trait Inlinable[-E]:

  /**
   * The result type of the inlining
   */
  type Result

  /**
   * Inlines the result of [[E]] in a [[Some]] if it can be computed; otherwise, inlines [[None]]
   * @return
   */
  inline def reduce: Option[Result]

  /**
   * Attempts to inline the value and errors at compile time if the value cannot be determined
   * @return the value extracted from [[E]]
   */
  transparent inline def inlined: Result =
    inline reduce match
      case Some(result) => result
      case None => compiletime.error("could not inline because the value is unknown")

object Inlinable:

  /**
   * Convenience method for inlining an expression
   * @param inlinable instance which which to attempt inlining
   * @tparam A the expression type being inlined
   * @return the reduced value ([[None]] if the value could not be determined)
   */
  transparent inline def reduce[A](using inlinable: Inlinable[A]): Option[inlinable.Result] = inlinable.reduce

  /**
   * Helper "constructor" for [[Inlinable]] that set the [[Result]] type to [[R]]
   * @tparam R the result type
   */
  trait Impl[R] extends Inlinable[Any]:
    override type Result = R

  /**
   * Type alias that is cleaner that the structural type.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Inlinable[E] { type Result <: R }

  /**
   * Type alias exposing the expression type.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type From[-E] = [R] =>> Typed[E, R]

  /**
   * Type alias exposing the [[Result]] type.
   *
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type To[+R] = [E] =>> Typed[E, R]

  type Predicate[-E] = Typed[E, Boolean]

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

  given Inlinable[Null] with
    override type Result = Null
    override transparent inline def reduce: Some[Null] = Some(null)

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a compile time computation that attempt to extract a result value from the result type
   */
  transparent inline given builtinSingleton[R <: Singleton: Builtin]: Inlinable.Typed[R, R] =
    builtin[R]

  inline given singleton[S <: Singleton]: Inlinable[S] with
    override type Result = S
    override transparent inline def reduce: Some[S] = Some(singletonToValue[S])

  private transparent inline def singletonToValue[S <: Singleton]: S =
    compiletime.summonInline[ValueOf[S]].value

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
    override transparent inline def reduce: Option[R] = ${ impl[R] }

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
    override transparent inline def reduce: Option[R] = ${ impl[R] }

  private def impl[E: Type](using Quotes): Expr[Option[E]] =
    given Builtin[E] = Builtin.evidenceOrAbort
    import constraints.compile.Builtin.toExpr
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
    import constraints.compile.Builtin.toExpr
    fromComputation(computable)

  /**
   * Attempts to extract the values from the expression and inline the result
   * @param computable computes the result
   * @param Quotes for performing macro operations
   * @tparam E the expression type to attempt extracting a value from
   * @tparam R the result type whose value can be inlined
   * @return the possible result value to inline
   */
  def fromComputation[E: Type: FromType, R: Type: ToExpr: ToType](
    computable: E => Compute.Typed[Nothing, R]
  )(using Quotes): Expr[Option[R]] =
    inlineOption(FromType[E].map(computable).map(_.compute))

  /**
   * Inlines the inlined result
   * @param possibleValue the value to inline
   * @param Quotes for performing macro operations
   * @tparam V the value to inline
   * @return an inlined value (which may be [[None]])
   */
  def inlineOption[V: Type: ToExpr: ToType](possibleValue: Option[V])(using Quotes): Expr[Option[V]] =
    possibleValue match
      case None => '{ None }
      case Some(value) =>
        val expr = Expr(value)
        val tpe = ToType(value)
        tpe match
          case '[e] =>
            val casted = '{ $expr.asInstanceOf[e] }.asExprOf[e]
            '{ Some[e]($casted) }.asExprOf[Option[V]]

  /**
   * Type class instance of [[Inlinable]] for [[and]]
   *
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[Inlinable]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Predicate[A and B] =
    inline Inlinable.reduce[A] match
      case Some(false) => Inlinable.Constant[false]
      case None => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None | Some(true) => Inlinable.Unknown
      case Some(true) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]

  /**
   * Type class instance of [[Inlinable]] for [[Not]]
   *
   * @tparam C the constraint to negate
   * @return a [[Inlinable]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given[C: Inlinable.To[Boolean]]: Inlinable.Predicate[Not[C]] =
    inline Inlinable.reduce[C] match
      case Some(false) => Inlinable.Constant[true]
      case None => Inlinable.Unknown
      case Some(true) => Inlinable.Constant[false]

  /**
   * Type class instance of [[Inlinable]] for [[or]]
   *
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[Inlinable]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Predicate[A or B] =
    inline Inlinable.reduce[A] match
      case Some(false) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case None => inline Inlinable.reduce[B] match
        case Some(false) | None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case Some(true) => Inlinable.Constant[true]

  /**
   * Type class instance of [[Inlinable]] for [[xor]]
   *
   * @tparam A the first constraint of the exclusive disjunction
   * @tparam B the second constraint of the exclusive disjunction
   * @return a [[Inlinable]] for the exclusive disjunction of [[A]] xor [[B]]
   *         either being unknown results in unknown
   *         both being known results in false if they are the same and true if they are different
   */
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Predicate[A xor B] =
    inline Inlinable.reduce[A] match
      case Some(false) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case None => Inlinable.Unknown
      case Some(true) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[true]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[false]
