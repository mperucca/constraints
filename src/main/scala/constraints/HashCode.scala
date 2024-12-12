package constraints

import scala.annotation.targetName

type HashCode[A]
@targetName("##")
type ##[A] = HashCode[A]

object HashCode extends Compute.UnaryCompanion[##, Any, Int](_.##)
