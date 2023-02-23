import constraints.*

type Fraction = Fraction.BlackBox

object Fraction:

  type BlackBox = Fraction.WhiteBox[Int, Int]

  class WhiteBox[+N <: Int, +D <: Int](val numerator: N, val denominator: D)(
    val nonZeroDenominator: Guarantee[denominator.type !== 0]
  )

  def apply(numerator: Int, denominator: Int)(
    nonZeroDenominator: Guarantee[denominator.type !== 0]
  ): Fraction.WhiteBox[numerator.type, denominator.type] =
    new Fraction.WhiteBox(numerator, denominator)(nonZeroDenominator)

  type Numerator[F <: Fraction.WhiteBox[? <: Singleton, ?]] = F match { case Fraction.WhiteBox[n, ?] => n }
  type Denominator[F <: Fraction.WhiteBox[?, ? <: Singleton]] = F match { case Fraction.WhiteBox[?, d] => d }
  type Tupled[F <: Fraction.WhiteBox[? <: Singleton, ? <: Singleton]] = F match { case Fraction.WhiteBox[n, d] => (n, d) }
