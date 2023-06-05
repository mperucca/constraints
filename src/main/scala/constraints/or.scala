package constraints

/**
 * Logical disjunction (sometimes represented by the symbol ∨)
 *
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait or[A, B]

object or:

  /**
   * The type class instance for the inclusive disjunction of constraints [[or]]
   *
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check succeeds)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if either runtime check succeeds
   */
  given[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Typed[A or B, Boolean] =
    Compute(Compute[A] || Compute[B])

  /**
   * Type class instance of [[Inlinable]] for [[or]]
   *
   * @param a the first [[Inlinable]] of the inclusive disjunction
   * @param b the second [[Inlinable]] of the inclusive disjunction
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[Inlinable]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Typed[A or B, Boolean] =
    inline Inlinable[A] match
      case Some(false) => inline Inlinable[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case None => inline Inlinable[B] match
        case Some(false) | None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case Some(true) => Inlinable.Constant[true]

