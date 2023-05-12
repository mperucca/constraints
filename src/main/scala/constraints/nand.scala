package constraints

/**
 * The NAND operator (sometimes represented by the symbol ⊼)
 *
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nand[A, B] = Not[A and B]
