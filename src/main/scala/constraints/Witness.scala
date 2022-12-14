package constraints

type Witness[A] = Witness.Impl[Normalize[A]]

object Witness:

  private[constraints] sealed trait Impl[+A]
  private[constraints] object Impl extends Impl[Nothing]

  def trust[A]: Witness[A] = Impl

  def runtimeCheck[A](using runtimeCheck: RuntimeCheck[A]): Either[Witness[not[A]], Witness[A]] =
    Either.cond(runtimeCheck.succeeded, trust, trust)

  inline def compileTimeCheck[A](using inline compileTimeCheck: CompileTimeCheck[A]): Witness[A] =
    inline compileTimeCheck.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => trust

  extension [A](witness: => Witness[A])

    infix def and[B](other: => Witness[B]): Witness[A and B] = trust
