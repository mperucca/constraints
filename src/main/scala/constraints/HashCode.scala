package constraints

import scala.annotation.targetName

type HashCode[A]
@targetName("##")
type ##[A] = HashCode[A]

object HashCode {

  given [A: Compute]: Compute.Typed[HashCode[A], Int] = Compute(Compute[A].##)
  
}
