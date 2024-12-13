package constraints

import scala.annotation.targetName

infix type Equals[A, B]
@targetName("Equals")
infix type ==[A, B] = Equals[A, B]

@targetName("UnequalTo")
infix type !=[A, B] = ![A == B]

object Equals extends Compute.BinaryCompanion[==, Any, Any, Boolean](_ == _)
