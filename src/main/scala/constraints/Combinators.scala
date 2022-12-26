package constraints

/**
 * Represents negation sometimes represented by the symbol ¬
 * @tparam A the term to negate
 */
sealed trait not[A]

/**
 * Logical conjunction sometimes represented by the symbol ∧
 * @tparam A the first term of the conjunction
 * @tparam B the second term of the conjunction
 */
infix sealed trait and[A, B]

/**
 * Logical disjunction sometimes represented by the symbol ∨
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait or[A, B]

/**
 * Exclusive disjunction sometimes represented by the symbol ⊕
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait xor[A, B]

/**
 * Material implication often represented by the symbol ⇒
 * @tparam A the antecedent term
 * @tparam B the consequent term
 */
infix type implies[A, B] = not[A] or B

/**
 * The NAND operator sometimes represented by the symbol ⊼
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nand[A, B] = not[A and B]

/**
 * The NOR operator somethime represented by the symbol ⊽
 * @tparam A the first term
 * @tparam B the second term
 */
infix type nor[A, B] = not[A or B]

/**
 * Material equivalence sometimes represented by the symbol ⇔
 * @tparam A the first term of the equivalence
 * @tparam B the second term of the equivalence
 */
infix type xnor[A, B] = not[A xor B]

/**
 * Universal quantification sometimes represented by the symbol ∀
 * @tparam T the set of terms
 * @tparam P the predicate that holds for each term
 */
type ForAll[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], true, and]

/**
 * Existential quantification sometimes represented by the symbol ∃
 * @tparam T the set of terms
 * @tparam P the predicate that holds for at least one term
 */
type Exists[T <: Tuple, P[_]] = Tuple.Fold[Tuple.Map[T, P], false, or]

/**
 * The inverse of a predicate
 * @tparam P the predicate to invert
 */
type Inverse[P[_]] = [X] =>> not[P[X]]
