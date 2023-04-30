package constraints

/**
 * Represents negation (sometimes represented by the symbol ¬)
 *
 * @tparam A the term to negate
 */
sealed trait Not[A]

/**
 * Logical conjunction (sometimes represented by the symbol ∧)
 *
 * @tparam A the first term of the conjunction
 * @tparam B the second term of the conjunction
 */
infix sealed trait and[A, B]

/**
 * Logical disjunction (sometimes represented by the symbol ∨)
 *
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait or[A, B]

/**
 * Exclusive disjunction (sometimes represented by the symbol ⊕)
 * @note Even though this could be a typed as {{{infix type xor[A, B] = (A and Not[B]) or (Not[A] and B)}}},
 *       using a separate trait means the implementation takes half the runtime checks should it fail
 *
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait xor[A, B]

/**
 * Material implication (often represented by the symbol ⇒)
 *
 * @tparam A the antecedent term
 * @tparam B the consequent term
 */
infix type implies[A, B] = Not[A] or B

/**
 * The NAND operator (sometimes represented by the symbol ⊼)
 *
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nand[A, B] = Not[A and B]

/**
 * The NOR operator (sometimes represented by the symbol ⊽)
 *
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nor[A, B] = Not[A or B]

/**
 * Material equivalence (sometimes represented by the symbol ⇔)
 *
 * @tparam A the first term of the equivalence
 * @tparam B the second term of the equivalence
 */
infix type xnor[A, B] = Not[A xor B]

/**
 * Universal quantification (sometimes represented by the symbol ∀)
 *
 * @tparam T the set of terms
 * @tparam C the constraint that holds for each term
 */
type ForAll[T <: Tuple, C[_]] = Tuple.Fold[Tuple.Map[T, C], true, and]

/**
 * Existential quantification (sometimes represented by the symbol ∃)
 *
 * @tparam T the set of terms
 * @tparam C the constraint that holds for at least one term
 */
type Exists[T <: Tuple, C[_]] = Tuple.Fold[Tuple.Map[T, C], false, or]

/**
 * The inverse of a predicate
 *
 * @tparam C the constraint to invert
 */
type Inverse[C[_]] = [X] =>> Not[C[X]]
