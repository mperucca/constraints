package constraints

/**
 * The NOR operator (sometimes represented by the symbol ⊽)
 *
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nor[A, B] = Not[A or B]
