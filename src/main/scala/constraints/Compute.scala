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

  def apply[E](using compute: Compute[E]): compute.Result = compute.compute

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Compute[E] { type Result <: R }

  type From[-E] = [R] =>> Typed[E, R]

  /**
   * Type alias for computations returning [[Boolean]]s
   * @tparam E the expression to compute
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
