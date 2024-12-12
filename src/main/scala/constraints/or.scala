package constraints

import scala.annotation.targetName

/**
 * Logical disjunction (sometimes represented by the symbol ∨)
 *
 * @tparam A the first term of the disjunction
 * @tparam B the second term of the disjunction
 */
infix sealed trait or[A, B]

@targetName("or")
type ||[A, B] = A or B

object or:

  /**
   * The type class instance for the inclusive disjunction of constraints [[or]]
   *
   * @tparam A the first constraint
   * @tparam B the second constraint
   * @return a runtime check that succeeds if either runtime check succeeds
   */
  given compute[A: Compute.To[Boolean], B: Compute.To[Boolean]]: Compute.Predicate[A or B] =
    Compute(Compute[A] || Compute[B])
