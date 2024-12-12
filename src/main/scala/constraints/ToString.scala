package constraints

type ToString[A]

object ToString extends Compute.UnaryCompanion[ToString, Any, String](_.toString)
