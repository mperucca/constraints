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
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A and B] =
    RuntimeComputation(a.result && b.result)

  /**
   * Type class instance of [[CompileTimeComputation]] for [[and]]
   *
   * @param a the first [[CompileTimeComputation]] of the conjunction
   * @param b the second [[CompileTimeComputation]] of the conjunction
   * @tparam A the first constraint of the conjunction
   * @tparam B the second constraint of the conjunction
   * @return a [[CompileTimeComputation]] for the conjunction of [[A]] and [[B]]
   *         either being false results in false
   *         neither being false and at least one being unknown results in unknown
   *         both being true results in true
   */
  transparent inline given[A, B](
    using a: CompileTimeComputation.Predicate[A], b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A and B] =
    inline a.result match
      case false => CompileTimeComputation.Constant[false]
      case null => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null | true => CompileTimeComputation.Unknown
      case true => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
