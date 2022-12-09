package constraints

type Proof[A] = Proof.Impl[Normalize[A]]

object Proof:

  private[constraints] sealed trait Impl[+A]
  private[constraints] object Impl extends Impl[Nothing]

  def unchecked[A]: Proof[A] = Impl

  def runtimeCheck[A: RuntimeCheck]: Option[Proof[A]] =
    Option.when(summon[RuntimeCheck[A]].succeeds)(unchecked)

  inline given compileTimeCheck[A](using inline c: CompileTimeCheck[A]): Proof[A] =
    inline c.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => unchecked

  inline def apply[A](using inline c: CompileTimeCheck[A]): Proof[A] = compileTimeCheck

  extension[A] (proof: => Proof[A])

    def corollaries[B](using => Corollary[A, B]): Proof[A And B] = unchecked

    def and[B](b: => Proof[B]): Proof[A And B] = unchecked
