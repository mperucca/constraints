package constraints

/**
 * The inverse of a predicate
 *
 * @tparam C the constraint to invert
 */
type Inverse[C[_]] = [X] =>> Not[C[X]]
