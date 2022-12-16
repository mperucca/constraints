import constraints.*

type Fraction = Fraction.WhiteBox[Int, Int]

object Fraction:

  type BlackBox = Fraction

  class WhiteBox[+N <: Int, +D <: Int](val numerator: N, val denominator: D)(
    val nonZeroDenominator: Witness[denominator.type !== 0]
  )

  def apply(numerator: Int, denominator: Int)(
    nonZeroDenominator: Witness[denominator.type !== 0]
  ): Fraction.WhiteBox[numerator.type, denominator.type] =
    new Fraction.WhiteBox(numerator, denominator)(nonZeroDenominator)

  type Numerator[F <: Fraction.WhiteBox[_ <: Singleton, _]] = F match { case Fraction.WhiteBox[n, _] => n }
  type Denominator[F <: Fraction.WhiteBox[_, _ <: Singleton]] = F match { case Fraction.WhiteBox[_, d] => d }
  type Tupled[F <: Fraction.WhiteBox[_ <: Singleton, _ <: Singleton]] = F match { case Fraction.WhiteBox[n, d] => (n, d) }
