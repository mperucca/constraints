import constraints.*

class Fraction(val numerator: Int, val denominator: Int)(
  val nonZeroDenominator: Guarantee[denominator.type !== 0]
)

object Fraction:

  type WhiteBox[N, D] = Fraction { val numerator: N; val denominator: D }

  def apply(numerator: Int, denominator: Int)(
    guarantee: Guarantee[denominator.type !== 0]
  ): Fraction.WhiteBox[numerator.type, denominator.type] =
    new Fraction(numerator, denominator)(guarantee).asInstanceOf

  type Numerator[F] = F match { case Fraction.WhiteBox[n, ?] => n }
  type Denominator[F] = F match { case Fraction.WhiteBox[?, d] => d }
  type Tupled[F] = F match { case Fraction.WhiteBox[n, d] => (n, d) }
