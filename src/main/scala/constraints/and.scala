package constraints

/**
 * Logical conjunction (sometimes represented by the symbol ∧)
 *
 * @tparam A the first term of the conjunction
 * @tparam B the second term of the conjunction
 */
infix sealed trait and[A, B]

object and:

  /**
   * The type class instance for the conjunction of constraints: [[and]]
   *
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if both runtime checks succeed
   */
  given[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Predicate[A and B] =
    Compute(Compute[A] && Compute[B])
