package constraint

class Refinement[+A, P[_]](val value: A)(val proof: Proof[P[value.type]])

object Refinement:

  def trust[P[_]](a: Any): Refinement[a.type, P] =
    Refinement(a)(Proof.trust[P[a.type]])

  def attempt[P[_]](a: Any)(using RuntimeCheck[P[a.type]]): Option[Refinement[a.type, P]] =
    Proof.attempt[P[a.type]].map(Refinement(a)(_))

  inline def prove[P[_]](a: Any)(using inline c: CompileTimeCheck[P[a.type]]): Refinement[a.type, P] =
    Refinement(a)(Proof[P[a.type]])
