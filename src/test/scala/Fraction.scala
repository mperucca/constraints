import constraints.*

sealed trait Fraction:
  val numerator: Int
  val denominator: Int
  val nonZeroDenominatorProof: Proof[denominator.type !== 0]
  type Tupled = (numerator.type, denominator.type)

object Fraction:

  type BlackBox = Fraction
  class WhiteBox[+N <: Int, +D <: Int](val numerator: N, val denominator: D)(
    val nonZeroDenominatorProof: Proof[denominator.type !== 0]
  ) extends Fraction

  def apply(numerator: Int, denominator: Int)(nonZeroDenominatorProof: Proof[denominator.type !== 0]): Fraction.WhiteBox[numerator.type, denominator.type] =
    new Fraction.WhiteBox(numerator, denominator)(nonZeroDenominatorProof)

  type Numerator[F <: Fraction] = F match { case Fraction.WhiteBox[n, _] => n }
  type Denominator[F <: Fraction] = F match { case Fraction.WhiteBox[_, d] => d }
  type Tupled[F <: Fraction] = F match { case Fraction.WhiteBox[n, d] => (n, d) }
