package constraints

opaque type RuntimeCheck[-A] = Boolean

object RuntimeCheck:

  def apply[A](succeeds: Boolean): RuntimeCheck[A] = succeeds
  extension (runtimeCheck: RuntimeCheck[Nothing])
    def succeeds: Boolean = runtimeCheck

  given RuntimeCheck[True] = true
  given RuntimeCheck[False] = false
  given[A](using check: RuntimeCheck[A]): RuntimeCheck[Not[A]] = !check
  given[A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A And B] = a && b
  given[A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A Or B] = a || b
  given[A, B](using a: RuntimeCheck[A], b: RuntimeCheck[B]): RuntimeCheck[A Xor B] = a != b
