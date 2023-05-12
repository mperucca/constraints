package constraints

/**
 * Existential quantification (sometimes represented by the symbol ∃)
 *
 * @tparam T the set of terms
 * @tparam C the constraint that holds for at least one term
 */
type Exists[T <: Tuple, C[_]] = Tuple.Fold[Tuple.Map[T, C], false, or]
