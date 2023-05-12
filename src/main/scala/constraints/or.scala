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
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A or B] =
    RuntimeComputation(a.result || b.result)

  /**
   * Type class instance of [[CompileTimeComputation]] for [[or]]
   *
   * @param a the first [[CompileTimeComputation]] of the inclusive disjunction
   * @param b the second [[CompileTimeComputation]] of the inclusive disjunction
   * @tparam A the first constraint of the inclusive disjunction
   * @tparam B the second constraint of the inclusive disjunction
   * @return a [[CompileTimeComputation]] for the inclusive disjunction of [[A]] or [[B]]
   *         either being true results in true
   *         neither being true and at least one being unknown results in unknown
   *         both being false results in false
   */
  transparent inline given[A, B](
    using inline a: CompileTimeComputation.Predicate[A], inline b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A or B] =
    inline a.result match
      case false => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case null => inline b.result match
        case false | null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case true => CompileTimeComputation.Constant[true]

