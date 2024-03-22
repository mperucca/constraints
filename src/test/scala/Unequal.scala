import constraints.*

import scala.annotation.targetName

infix type Unequal[A, B] = Not[Equal[A, B]]
@targetName("Unequal")
type !==[A, B] = Unequal[A, B]
