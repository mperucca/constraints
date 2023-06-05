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
  given[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Typed[A xor B, Boolean] =
    Compute(Compute[A] != Compute[B])

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
  transparent inline given[A: Inlinable.To[Boolean], B: Inlinable.To[Boolean]]: Inlinable.Typed[A xor B, Boolean] =
    inline Inlinable.reduce[A] match
      case Some(false) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[false]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[true]
      case None => Inlinable.Unknown
      case Some(true) => inline Inlinable.reduce[B] match
        case Some(false) => Inlinable.Constant[true]
        case None => Inlinable.Unknown
        case Some(true) => Inlinable.Constant[false]