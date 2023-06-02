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
   * Type class instance of [[Inliner]] for [[and]]
   *
   * @param a the first [[Inliner]] of the conjunction
   * @param b the second [[Inliner]] of the conjunction
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[Inliner]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
  transparent inline given[A, B](using a: Inliner.Predicate[A], b: Inliner.Predicate[B]): Inliner.Predicate[A and B] =
    inline a.reduce match
      case false => Inliner.Constant[false]
      case null => inline b.reduce match
        case false => Inliner.Constant[false]
        case null | true => Inliner.Unknown
      case true => inline b.reduce match
        case false => Inliner.Constant[false]
        case null => Inliner.Unknown
        case true => Inliner.Constant[true]
