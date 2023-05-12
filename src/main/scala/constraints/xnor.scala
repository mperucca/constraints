package constraints

/**
 * Material equivalence (sometimes represented by the symbol ⇔)
 *
 * @tparam A the first term of the equivalence
 * @tparam B the second term of the equivalence
 */
infix type xnor[A, B] = Not[A xor B]
