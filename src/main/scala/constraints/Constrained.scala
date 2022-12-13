package constraints

import scala.collection.IterableOps

class Constrained[+A, P[_ <: A]](val value: A)(val proof: Proof[P[value.type]])

object Constrained:

  def apply[P[_]](a: Any)(proof: Proof[P[a.type]]): Constrained[a.type, P] = new Constrained(a)(proof)

  def apply(a: Any): [P[_ <: a.type]] => Proof[P[a.type]] => Constrained[a.type, P] =
    [P[_ <: a.type]] => (proof: Proof[P[a.type]]) => new Constrained[a.type, P](a)(proof)

  def partition[I[_], A, P[_]](iterable: I[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]])(using I[A] <:< IterableOps[A, I, I[A]]): (I[A Constrained Inverse[P]], I[A Constrained P]) =
    iterable.partitionMap { a =>
      Proof.checkAtRuntime(using runtimeCheck(a)) match
        case Left(negativeProof: Proof[Not[P[a.type]]]) => Left(new Constrained(a)(negativeProof))
        case Right(proof: Proof[P[a.type]]) => Right(new Constrained(a)(proof))
    }
