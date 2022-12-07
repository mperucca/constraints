package constraint

import scala.util.NotGiven

type Proof[A] = A match
  case Not[Not[a]] => Proof[a]
  case Not[a And b] => Proof[Not[a] Or Not[b]]
  case Not[a Or b] => Proof[Not[a] And Not[b]]
  case Not[a Xor b] => Proof[Not[TranslateXor[a, b]]]
  case a And b => Proof[a] & Proof[b]
  case a Or b => Proof[a] | Proof[b]
  case a Xor b => Proof[TranslateXor[a, b]]
  case Not[False] => True
  case Not[True] => False
  case _ => A
private type TranslateXor[A, B] = (A And Not[B]) Or (Not[A] And B)

object Proof:

  def trust[A]: Proof[A] = null.asInstanceOf[Proof[A]]

  def attempt[A: RuntimeCheck]: Option[Proof[A]] =
    Option.when(summon[RuntimeCheck[A]].succeeds)(trust[A])

  inline def apply[A](using inline c: CompileTimeCheck[A]): Proof[A] =
    inline c.satisfied match
      case false => compiletime.error("invalid")
      case null => compiletime.error("unknown")
      case true => trust[A]

  def and[A, B](a: A, b: B): Proof[A And B] = trust[A And B]
