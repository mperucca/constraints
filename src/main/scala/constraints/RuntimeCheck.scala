package constraints

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

  def all[A, P[_]](iterable: Iterable[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]]): (Iterable[A Refinement Inverse[P]], Iterable[A Refinement P]) =
    iterable.partitionMap { a =>
      Proof.runtimeCheck(using runtimeCheck(a)) match
        case Left(given Proof[Not[P[a.type]]]) => Left(Refinement(a))
        case Right(given Proof[P[a.type]]) => Right(Refinement(a))
    }
