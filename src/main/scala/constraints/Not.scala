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
  given[C](using c: Computable.Predicate[C]): Computable.Predicate[Not[C]] =
    Computable(!c.compute)

  /**
   * Type class instance of [[Inlinable]] for [[Not]]
   *
   * @param c the [[Inlinable]] instance to negate
   * @tparam C the constraint to negate
   * @return a [[Inlinable]] for the negation of [[C]]
   *         [[Not]] on false becomes true
   *         [[Not]] on unknown stays unknown
   *         [[Not]] on true becomes false
   */
  transparent inline given[C](using c: Inlinable.Predicate[C]): Inlinable.Predicate[Not[C]] =
    inline c.reduce match
      case false => Inlinable.Constant[true]
      case null => Inlinable.Unknown
      case true => Inlinable.Constant[false]