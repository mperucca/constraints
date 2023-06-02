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
  given[A, B](using a: Computation.Predicate[A], b: Computation.Predicate[B]): Computation.Predicate[A xor B] =
    Computation(a.compute != b.compute)

  /**
   * Type class instance of [[Inlinable]] for [[xor]]
   *
   * @param a the first [[Inlinable]] of the exclusive disjunction
   * @param b the second [[Inlinable]] of the exclusive disjunction
   * @tparam A the first constraint of the exclusive disjunction
   * @tparam B the second constraint of the exclusive disjunction
   * @return a [[Inlinable]] for the exclusive disjunction of [[A]] xor [[B]]
   *         either being unknown results in unknown
   *         both being known results in false if they are the same and true if they are different
   */
  transparent inline given[A, B](using a: Inlinable.Predicate[A], b: Inlinable.Predicate[B]): Inlinable.Predicate[A xor B] =
    inline a.reduce match
      case false => inline b.reduce match
        case false => Inlinable.Constant[false]
        case null => Inlinable.Unknown
        case true => Inlinable.Constant[true]
      case null => Inlinable.Unknown
      case true => inline b.reduce match
        case false => Inlinable.Constant[true]
        case null => Inlinable.Unknown
        case true => Inlinable.Constant[false]