package constraints

class Refinement[+A, P[_ <: A]](val value: A)(using val proof: Proof[P[value.type]])

object Refinement:

  def apply(a: Any): [P[_ <: a.type]] => Proof[P[a.type]] ?=> Refinement[a.type, P] =
    [P[_ <: a.type]] => (proof: Proof[P[a.type]]) ?=> new Refinement[a.type, P](a)
