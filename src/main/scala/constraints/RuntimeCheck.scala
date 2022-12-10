package constraints

import scala.collection.IterableOps

opaque type RuntimeCheck[-A] = Boolean

object RuntimeCheck:

  def apply[A](succeeds: Boolean): RuntimeCheck[A] = succeeds
  extension (runtimeCheck: RuntimeCheck[Nothing])
    def succeeds: Boolean = runtimeCheck

  given RuntimeCheck[True] = true
  given RuntimeCheck[False] = false
  given [A](using check: RuntimeCheck[A]): RuntimeCheck[Not[A]] = !check
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A And B] = a && b
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A Or B] = a || b
  given [A, B](using a: RuntimeCheck[A], b: RuntimeCheck[B]): RuntimeCheck[A Xor B] = a != b

  def all[I[_], A, P[_]](iterable: I[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]])(using I[A] <:< IterableOps[A, I, I[A]]): (I[A Refinement Inverse[P]], I[A Refinement P]) =
    iterable.partitionMap { a =>
      Proof.checkAtRuntime(using runtimeCheck(a)) match
        case Left(given Proof[Not[P[a.type]]]) => Left(Refinement(a))
        case Right(given Proof[P[a.type]]) => Right(Refinement(a))
    }
