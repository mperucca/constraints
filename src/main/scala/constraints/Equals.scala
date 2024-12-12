package constraints

import scala.annotation.targetName

infix type Equals[A, B]
@targetName("Equals")
infix type ==[A, B] = Equals[A, B]

infix type UnequalTo[A, B] = Not[A Equals B]
@targetName("UnequalTo")
infix type !=[A, B] = Not[A == B]

object Equals extends Compute.BinaryCompanion[==, Any, Any, Boolean](_ == _)
