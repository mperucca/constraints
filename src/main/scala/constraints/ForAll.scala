package constraints

/**
 * Universal quantification (sometimes represented by the symbol ∀)
 *
 * @tparam T the set of terms
 * @tparam C the constraint that holds for each term
 */
type ForAll[T <: Tuple, C[_]] = Tuple.Fold[Tuple.Map[T, C], true, and]
