package constraints

import scala.annotation.targetName

infix trait EqualTo[-A, -B]:
  def apply(a: A, b: B): Boolean
@targetName("EqualTo")
type `=`[A, B] = EqualTo[A, B]

object EqualTo {

  def fromEquiv[A: Equiv]: EqualTo[A, A] = Equiv[A].equiv(_, _)

  given boolean: EqualTo[Boolean, Boolean] = _ == _

  given byte: EqualTo[Byte, Byte] = _ == _

  given char: EqualTo[Char, Char] = _ == _

  given double: EqualTo[Double, Double] = _ == _

  given float: EqualTo[Float, Float] = _ == _

  given int: EqualTo[Int, Int] = _ == _

  given long: EqualTo[Long, Long] = _ == _

  given short: EqualTo[Short, Short] = _ == _

  given string: EqualTo[String, String] = _ == _

  given emptyTuple: EqualTo[EmptyTuple, EmptyTuple] = _ == _

  given nonEmptyTuple[H1, T1 <: Tuple, H2, T2 <: Tuple](
    using head: EqualTo[H1, H2], tail: EqualTo[T1, T2]
  ): EqualTo[H1 *: T1, H2 *: T2] =
    case (h1 *: t1, h2 *: t2) => head(h1, h2) && tail(t1, t2)

  given option[A, B](using equalTo: EqualTo[A, B]): EqualTo[Option[A], Option[B]] =
    case (None, None) => true
    case (Some(a), Some(b)) => equalTo(a, b)
    case _ => false

  given compute[A, B](using a: Compute[A], b: Compute[B])(
    using equalTo: EqualTo[a.Result, b.Result]
  ): Compute.Typed[EqualTo[A, B], Boolean] =
    Compute(equalTo(Compute[A], Compute[B]))

}
