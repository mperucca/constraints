package constraints

/**
 * Type class for computing [[E]] at runtime.
 * Only the input is exposed as a type parameter so that the output [[Result]] is inferred.
 */
trait Computable[-E]:

  /**
   * The result type when [[E]] is computed
   */
  type Result

  /**
   * The result of computing [[E]]
   * @return the computed result
   */
  def compute: Result

object Computable:

  /**
   * Type alias exposing the [[Result]] type to support the Aux pattern.
   * @tparam E the expression to compute
   * @tparam R the computation result
   */
  type Typed[-E, +R] = Computable[E] { type Result <: R }

  /**
   * Type alias for computations returning [[Boolean]]s
   * @tparam E the expression to compute
   */
  type Predicate[-E] = Typed[E, Boolean]

  /**
   * Helper function to construct a runtime computation with a lazily computed result
   * @param computation the computation to run
   * @tparam E the expression type to which this computation result belongs
   * @tparam R the result type
   * @return the runtime computation instance
   */
  def apply[E, R](computation: => R): Computable.Typed[E, R] =
    new Computable[E] {
      override type Result = R
      override def compute: R = computation
    }

  /**
   * Type class instance to infer singleton types instead of their widened types when possible
   * @tparam R the result type from which to extract a value
   * @return a runtime computation that uses [[ValueOf]]
   */
  given literalSingleton[R <: Singleton: ValueOf]: Computable.Typed[R, R] =
    value

  /**
   * Type class instance for values encoded in types
   * @note unsure if this is needed
   * @tparam R the result type from which to extract a value
   * @return the runtime computation that uses [[ValueOf]]
   */
  given value[R: ValueOf]: Computable.Typed[R, R] =
    Computable(valueOf[R])
