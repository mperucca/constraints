package constraints

type Proof[A] = Proof.Impl[Normalize[A]]

object Proof:

  private[constraints] sealed trait Impl[+A]
  private[constraints] object Impl extends Impl[Nothing]

  def unchecked[A]: Proof[A] = Impl

  def checkAtRuntime[A](using runtimeCheck: RuntimeCheck[A]): Either[Proof[Not[A]], Proof[A]] =
    Either.cond(runtimeCheck.succeeds, unchecked, unchecked)

  inline given checkAtCompileTime[A](using inline compileTimeCheck: CompileTimeCheck[A]): Proof[A] =
    inline compileTimeCheck.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => unchecked

  inline def apply[A](using inline c: CompileTimeCheck[A]): Proof[A] = checkAtCompileTime

  extension [A](proof: => Proof[A])

    def and[B](other: => Proof[B]): Proof[A And B] = unchecked
