package constraints

class Refinement[+A, P[_]](val value: A)(using val proof: Proof[P[value.type]])

object Refinement:

  def trust[P[_]](a: Any): Refinement[a.type, P] =
    Refinement(a)(using Proof.unchecked)

  def runtimeCheck[P[_]](a: Any)(using RuntimeCheck[P[a.type]]): Option[Refinement[a.type, P]] =
    Proof.runtimeCheck[P[a.type]].map(Refinement(a)(using _))

  inline def prove[P[_]](a: Any)(using inline c: CompileTimeCheck[P[a.type]]): Refinement[a.type, P] =
    Refinement(a)(using Proof[P[a.type]])
