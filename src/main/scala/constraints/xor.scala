package constraints

/**
 * Exclusive disjunction (sometimes represented by the symbol ⊕)
 *
 * @note Even though this could be a typed as {{{infix type xor[A, B] = (A and Not[B]) or (Not[A] and B)}}},
 *       using a separate trait means the implementation takes half the runtime checks should it fail
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait xor[A, B]

object xor:

  /**
   * The type class instance for the exclusive disjunction of constraints [[xor]]
   *
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return the runtime check that succeeds if one but not both of the runtime checks succeeds
   */
  given[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Typed[A xor B, Boolean] =
    Compute(Compute[A] != Compute[B])
