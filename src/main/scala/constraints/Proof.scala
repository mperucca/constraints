package constraints

type Proof[A] = Proof.Impl[Normalize[A]]

object Proof:

  sealed trait Impl[+A]

  object Impl extends Impl[Nothing]:

    extension [A](proof: => Impl[A])

      def corollaries[B](using => Corollary[A, B]): Proof[A And B] = unchecked[A And B]

  def unchecked[A]: Proof[A] = Impl

  def runtimeCheck[A: RuntimeCheck]: Option[Proof[A]] =
    Option.when(summon[RuntimeCheck[A]].succeeds)(unchecked[A])

  inline given compileTimeCheck[A](using inline c: CompileTimeCheck[A]): Proof[A] =
    inline c.valid match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => unchecked[A]

  inline def apply[A](using inline c: CompileTimeCheck[A]): Proof[A] = compileTimeCheck

  // Making this an extension method results in the compiler hanging...
  def and[A, B](a: Proof[A], b: Proof[B]): Proof[A And B] = unchecked[A And B]
