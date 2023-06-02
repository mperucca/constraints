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
  given[A, B](using a: Computation.Predicate[A], b: Computation.Predicate[B]): Computation.Predicate[A and B] =
    Computation(a.compute && b.compute)

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
  transparent inline given[A, B](using a: Inlinable.Predicate[A], b: Inlinable.Predicate[B]): Inlinable.Predicate[A and B] =
    inline a.reduce match
      case false => Inlinable.Constant[false]
      case null => inline b.reduce match
        case false => Inlinable.Constant[false]
        case null | true => Inlinable.Unknown
      case true => inline b.reduce match
        case false => Inlinable.Constant[false]
        case null => Inlinable.Unknown
        case true => Inlinable.Constant[true]
