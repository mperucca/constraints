package constraints

/**
 * Exclusive disjunction (sometimes represented by the symbol ⊕)
 * @note Even though this could be a typed as {{{infix type xor[A, B] = (A and Not[B]) or (Not[A] and B)}}},
 *       using a separate trait means the implementation takes half the runtime checks should it fail
 *
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait xor[A, B]

object xor:

  /**
   * The type class instance for the exclusive disjunction of constraints [[xor]]
   *
   * @param a a runtime check for the first constraint
   * @param b a runtime check for the second constraint
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return the runtime check that succeeds if one but not both of the runtime checks succeeds
   */
  given[A, B](
    using a: RuntimeComputation.Predicate[A], b: RuntimeComputation.Predicate[B]
  ): RuntimeComputation.Predicate[A xor B] =
    RuntimeComputation(a.result != b.result)

  /**
   * Type class instance of [[CompileTimeComputation]] for [[xor]]
   *
   * @param a the first [[CompileTimeComputation]] of the exclusive disjunction
   * @param b the second [[CompileTimeComputation]] of the exclusive disjunction
   * @tparam A the first constraint of the exclusive disjunction
   * @tparam B the second constraint of the exclusive disjunction
   * @return a [[CompileTimeComputation]] for the exclusive disjunction of [[A]] xor [[B]]
   *         either being unknown results in unknown
   *         both being known results in false if they are the same and true if they are different
   */
  transparent inline given[A, B](
    using inline a: CompileTimeComputation.Predicate[A], inline b: CompileTimeComputation.Predicate[B]
  ): CompileTimeComputation.Predicate[A xor B] =
    inline a.result match
      case false => inline b.result match
        case false => CompileTimeComputation.Constant[false]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[true]
      case null => CompileTimeComputation.Unknown
      case true => inline b.result match
        case false => CompileTimeComputation.Constant[true]
        case null => CompileTimeComputation.Unknown
        case true => CompileTimeComputation.Constant[false]