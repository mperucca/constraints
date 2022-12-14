package constraints

import scala.collection.IterableOps

class Constrained[+A, P[_ <: A]](val value: A)(val witness: Witness[P[value.type]])

object Constrained:

  def apply[P[_]](a: Any)(witness: Witness[P[a.type]]): Constrained[a.type, P] = new Constrained(a)(witness)

  def apply(a: Any): [P[_ <: a.type]] => Witness[P[a.type]] => Constrained[a.type, P] =
    [P[_ <: a.type]] => (witness: Witness[P[a.type]]) => new Constrained[a.type, P](a)(witness)

  def partition[I[_], A, P[_]](iterable: I[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]])(
    using I[A] <:< IterableOps[A, I, I[A]]
  ): (I[A Constrained Inverse[P]], I[A Constrained P]) =
    iterable.partitionMap { a =>
      Witness.runtimeCheck(using runtimeCheck(a)) match
        case Left(negativeWitness: Witness[not[P[a.type]]]) => Left(new Constrained(a)(negativeWitness))
        case Right(witness: Witness[P[a.type]]) => Right(new Constrained(a)(witness))
    }
