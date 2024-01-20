package constraints

/**
 * Type class for computing [[E]] at runtime.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 */
trait Compute[-E]:

  /**
   * The result type when [[E]] is computed
   */
  type Result

  /**
   * The result of computing [[E]]
   * @return the computed result
   */
  def compute: Result

object Compute:

  /**
   * Evaluates the computation
   * @param compute computes to result
   * @tparam E The type which must be computable with a [[Compute]] instance
   * @return the computed result
   */
  def apply[E](using compute: Compute[E]): compute.Result = compute.compute

  /**
   * Type alias for cleaner [[Compute]] type signatures
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Compute[E] { type Result <: R }

  /**
   * Type alias partially applying the expression type
   * @tparam E the expression to compute
   */
  type From[-E] = [R] =>> Typed[E, R]

  /**
   * Type alias partially applying the result type
   * @tparam R the expression to compute
   */
  type To[+R] = [E] =>> Typed[E, R]

  /**
   * Helper function to construct a runtime computation with a lazily computed result
   * @param computation the computation to run
   * @tparam E the expression type to which this computation result belongs
   * @tparam R the result type
   * @return the runtime computation instance
   */
  def apply[E, R](computation: => R): Compute.Typed[E, R] =
    new Compute[E]:
      override type Result = R
      override def compute: R = computation

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a runtime computation that uses [[ValueOf]]
   */
  given literalSingleton[R <: Singleton: ValueOf]: Compute.Typed[R, R] =
    value

  /**
   * Type class instance for values encoded in types
   * @note unsure if this is needed
   * @tparam R the result type from which to extract a value
   * @return the runtime computation that uses [[ValueOf]]
   */
  given value[R: ValueOf]: Compute.Typed[R, R] =
    Compute(valueOf[R])

  /**
   * Type class instance for [[Null]]
   * @return the [[Compute]] instance for the constant null
   */
  given nullCompute: Compute.Typed[Null, Null] = Compute(null)

  given someCompute[A](using compute: Compute[A]): Compute.Typed[Some[A], Some[compute.Result]] =
    Compute(Some(Compute[A]))

  given nonEmptyTupleCompute[H, T <: Tuple](
    using head: Compute[H], tail: Compute.Typed[T, Tuple]
  ): Compute.Typed[H *: T, head.Result *: tail.Result] =
    Compute(head.compute *: tail.compute)
