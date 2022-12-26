package constraints

import scala.collection.IterableOps

class Constrained[+A, P[_ <: A]](val value: A)(val trust: Trust[P[value.type]])

object Constrained:

  def apply[P[_]](a: Any)(trust: Trust[P[a.type]]): Constrained[a.type, P] = new Constrained(a)(trust)

  def apply(a: Any): [P[_ <: a.type]] => Trust[P[a.type]] => Constrained[a.type, P] =
    [P[_ <: a.type]] => (trust: Trust[P[a.type]]) => new Constrained[a.type, P](a)(trust)

  def partition[I[_], A, P[_]](iterable: I[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]])(
    using I[A] <:< IterableOps[A, I, I[A]]
  ): (I[A Constrained Inverse[P]], I[A Constrained P]) =
    iterable.partitionMap { a =>
      Trust.runtimeCheck(using runtimeCheck(a)) match
        case Left(distrust: Trust[not[P[a.type]]]) => Left(new Constrained(a)(distrust))
        case Right(trust: Trust[P[a.type]]) => Right(new Constrained(a)(trust))
    }
