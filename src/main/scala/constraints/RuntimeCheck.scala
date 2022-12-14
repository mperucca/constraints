package constraints

opaque type RuntimeCheck[-A] = Boolean

object RuntimeCheck:

  def apply[A](succeeded: Boolean): RuntimeCheck[A] = succeeded
  extension (runtimeCheck: RuntimeCheck[Nothing])
    def succeeded: Boolean = runtimeCheck

  given success: RuntimeCheck[true] = true
  given failure: RuntimeCheck[false] = false
  given [A](using a: RuntimeCheck[A]): RuntimeCheck[not[A]] = !a
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A and B] = a && b
  given [A, B](using a: RuntimeCheck[A], b: => RuntimeCheck[B]): RuntimeCheck[A or B] = a || b
  given [A, B](using a: RuntimeCheck[A], b: RuntimeCheck[B]): RuntimeCheck[A xor B] = a != b
