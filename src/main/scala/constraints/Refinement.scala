package constraints

class Refinement[+A, P[_]](val value: A)(using val proof: Proof[P[value.type]])

object Refinement:

  def unchecked[P[_]](a: Any): Refinement[a.type, P] =
    new Refinement(a)(using Proof.unchecked)

  def runtimeCheck[P[_]](a: Any)(using RuntimeCheck[P[a.type]]): Option[Refinement[a.type, P]] =
    Proof.runtimeCheck[P[a.type]].map(new Refinement(a)(using _))

  inline def compileTimeCheck[P[_]](a: Any)(using inline c: CompileTimeCheck[P[a.type]]): Refinement[a.type, P] =
    new Refinement(a)(using Proof[P[a.type]])

  inline def apply[P[_]](a: Any)(using inline c: CompileTimeCheck[P[a.type]]): Refinement[a.type, P] =
    compileTimeCheck(a)
