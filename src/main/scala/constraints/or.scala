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
  given[A, B](using a: Computation.Predicate[A], b: Computation.Predicate[B]): Computation.Predicate[A or B] =
    Computation(a.compute || b.compute)

  /**
   * Type class instance of [[Inliner]] for [[or]]
   *
   * @param a the first [[Inliner]] of the inclusive disjunction
   * @param b the second [[Inliner]] of the inclusive disjunction
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[Inliner]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
  transparent inline given[A, B](using a: Inliner.Predicate[A], b: Inliner.Predicate[B]): Inliner.Predicate[A or B] =
    inline a.reduce match
      case false => inline b.reduce match
        case false => Inliner.Constant[false]
        case null => Inliner.Unknown
        case true => Inliner.Constant[true]
      case null => inline b.reduce match
        case false | null => Inliner.Unknown
        case true => Inliner.Constant[true]
      case true => Inliner.Constant[true]

