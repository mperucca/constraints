package constraints

/**
 * Represents negation (sometimes represented by the symbol ¬)
 *
 * @tparam A the term to negate
 */
sealed trait Not[A]

object Not:

  /**
   * The type class instance for the negation of constraints: [[Not]]
   *
   * @tparam C the constraint
   * @return a runtime check that succeeds if the runtime check for [[C]] fails
   */
  given compute[C: Compute.To[Boolean]]: Compute.Predicate[Not[C]] =
    Compute(!Compute[C])
