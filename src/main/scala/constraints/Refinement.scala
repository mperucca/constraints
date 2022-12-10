package constraints

class Refinement[+A, P[_]](val value: A)(using val proof: Proof[P[value.type]])

object Refinement:
  
  def apply[P[_]](a: Any)(using Proof[P[a.type]]): Refinement[a.type, P] = new Refinement(a)
