package constraints

type ToString[A]

object ToString {
  
  given [A: Compute]: Compute.Typed[ToString[A], String] = Compute(Compute[A].toString)
  
}
