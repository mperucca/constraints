package constraints

import scala.annotation.targetName

/**
 * Represents negation (sometimes represented by the symbol ¬)
 *
 * @tparam A the term to negate
 */
sealed trait Not[A]

@targetName("Not")
type ![A] = Not[A]

object Not extends Compute.UnaryCompanion[!, Boolean, Boolean](!_)
