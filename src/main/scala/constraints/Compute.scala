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
   * Type alias for [[Compute]] with [[Boolean]] result type
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
   * Type class instance for [[Some]]
   * @param compute computes the contained [[A]]
   * @tparam A the type of the value inside the [[Some]]
   * @return the runtime computation for [[Some]] of [[A]]
   */
  given some[A](using compute: Compute[A]): Compute.Typed[Some[A], Some[compute.Result]] =
    Compute(Some(Compute[A]))

  /**
   * Typle class instance for [[*:]] (the sub class of [[NonEmptyTuple]])
   * @param head computes the head
   * @param tail computes the tail
   * @tparam H the type of the head
   * @tparam T the type of the tail
   * @return the runtime computation for non-empty tuples
   */
  given nonEmptyTuple[H, T <: Tuple](
    using head: Compute[H], tail: Compute.Typed[T, Tuple]
  ): Compute.Typed[H *: T, head.Result *: tail.Result] =
    Compute(head.compute *: tail.compute)

  /**
   * Helper companion for simple unary [[Compute]] type class instances
   * @tparam C the constraint
   * @tparam E the input type
   * @tparam R the output type
   */
  trait UnaryCompanion[C[_], -E, +R](computation: E => R) {
    given compute[A: Compute.To[E]]: Compute.Typed[C[A], R] = Compute(computation(Compute[A]))
  }

  /**
   * Helper companion for simple binary [[Compute]] type class instances
   * @note This eager evaluates the arguments so is inadequate for short-circuiting such as in [[and]]/[[or]]
   * @tparam C the constraint
   * @tparam E1 the first input type
   * @tparam E2 the second input type
   * @tparam R the output type
   */
  trait BinaryCompanion[C[_, _], -E1, -E2, +R](computation: (E1, E2) => R) {
    given compute[A: Compute.To[E1], B: Compute.To[E2]]: Compute.Typed[C[A, B], R] =
      Compute(computation(Compute[A], Compute[B]))
  }
