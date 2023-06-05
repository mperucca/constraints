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
   * @param a runtime check for the first constraint
   * @param b runtime check for the second constraint (by-name as it's unnecessary if the first runtime check fails)
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if both runtime checks succeed
   */
  given[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Typed[A and B, Boolean] =
    Compute(Compute[A] && Compute[B])

  /**
   * Type class instance of [[Inlinable]] for [[and]]
   *
   * @param a the first [[Inlinable]] of the conjunction
   * @param b the second [[Inlinable]] of the conjunction
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[Inlinable]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Typed[A and B, Boolean] =
    inline Inlinable.reduce[A] match
      case Some(false) => Inlinable.Constant[false]
      case None => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None | Some(true) => Inlinable.Unknown
      case Some(true) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
