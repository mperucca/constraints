package constraints

/**
 * Type class for checking constraint [[C]] at runtime
 */
opaque type RuntimeCheck[-C] = Boolean

/**
 * Constructor for [[RuntimeCheck]] instances and various type class instances for [[RuntimeCheck]]
 */
object RuntimeCheck:

  /**
   * Constructs a [[RuntimeCheck]] with the provided result
   *
   * @param succeeds whether or not the runtime check passes
   * @tparam C the constraint type
   * @return the [[RuntimeCheck]] value
   */
  def apply[C](succeeds: Boolean): RuntimeCheck[C] = succeeds

  extension (runtimeCheck: RuntimeCheck[Nothing])

    /**
     * Unwraps the result from the [[RuntimeCheck]]
     */
    def succeeded: Boolean = runtimeCheck

  /**
   * A type class instance that always returns true
   *
   * @return the [[RuntimeCheck]] instance for true
   */
  given success: RuntimeCheck[true] = true

  /**
   * A type class instance that always returns false
   *
   * @return the [[RuntimeCheck]] instance for false
   */
  given failure: RuntimeCheck[false] = false

  /**
   * The type class instance for the negation of constraints: [[Not]]
   *
   * @param runtimeCheck the runtime check to invert
   * @tparam C the constraint
   * @return a runtime check that succeeds if the runtime check for [[C]] fails
   */
  given [C](using runtimeCheck: RuntimeCheck[C]): RuntimeCheck[Not[C]] = !runtimeCheck

  /**
   * The type class instance for the conjunction of constraints: [[and]]
   *
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check fails)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if both runtime checks succeed
   */
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A and B] = a && b

  /**
   * The type class instance for the inclusive disjunction of constraints [[or]]
   *
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check succeeds)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if either runtime check succeeds
   */
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A or B] = a || b

  /**
   * The type class instance for the exclusive disjunction of constraints [[xor]]
   *
   * @param a a runtime check for the first constraint
   * @param b a runtime check for the second constraint
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return the runtime check that succeeds if one but not both of the runtime checks succeeds
   */
  given [A, B](using a: RuntimeCheck[A], b: RuntimeCheck[B]): RuntimeCheck[A xor B] = a != b
