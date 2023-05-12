package constraints

/**
 * Material implication (often represented by the symbol ⇒)
 *
 * @tparam A the antecedent term
 * @tparam B the consequent term
 */
infix type implies[A, B] = Not[A] or B
