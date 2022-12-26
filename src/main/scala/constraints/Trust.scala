package constraints

type Trust[A] = Trust.Impl[Normalize[A]]

object Trust:

  private[constraints] sealed trait Impl[+A]
  private[constraints] object Impl extends Impl[Nothing]

  def belief[A]: Trust[A] = Impl

  def runtimeCheck[A](using runtimeCheck: RuntimeCheck[A]): Either[Trust[not[A]], Trust[A]] =
    Either.cond(runtimeCheck.succeeded, belief, belief)

  inline def compileTimeCheck[A](using inline compileTimeCheck: CompileTimeCheck[A]): Trust[A] =
    inline compileTimeCheck.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => belief

  extension [A](trust: => Trust[A])

    infix def and[B](other: => Trust[B]): Trust[A and B] = belief
