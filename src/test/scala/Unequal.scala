import constraints.*

import scala.annotation.targetName

type Unequal[A, B] = Not[Equal[A, B]]
@targetName("Unequal")
type !==[A, B] = Not[A === B]
