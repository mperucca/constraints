import constraints.*

import scala.annotation.targetName

type Unequal[A, B] = not[Equal[A, B]]
@targetName("Unequal")
type !==[A, B] = not[A === B]
