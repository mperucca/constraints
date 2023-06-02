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
   * @param c the runtime check to invert
   * @tparam C the constraint
   * @return a runtime check that succeeds if the runtime check for [[C]] fails
   */
  given[C](using c: Computation.Predicate[C]): Computation.Predicate[Not[C]] =
    Computation(!c.compute)

  /**
   * Type class instance of [[Inliner]] for [[Not]]
   *
   * @param c the [[Inliner]] instance to negate
   * @tparam C the constraint to negate
   * @return a [[Inliner]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given[C](using c: Inliner.Predicate[C]): Inliner.Predicate[Not[C]] =
    inline c.reduce match
      case false => Inliner.Constant[true]
      case null => Inliner.Unknown
      case true => Inliner.Constant[false]