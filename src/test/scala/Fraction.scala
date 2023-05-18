import constraints.*

type Fraction = Fraction.BlackBox

object Fraction:

  type BlackBox = Fraction.WhiteBox[Int, Int]

  class WhiteBox[+N <: Int, +D <: Int](val numerator: N, val denominator: D)(
    val nonZeroDenominator: Guarantee[denominator.type !== 0]
  )

  def apply(numerator: Int, denominator: Int)(guarantee: Guarantee[denominator.type !== 0]) =
    new Fraction.WhiteBox[numerator.type, denominator.type](numerator, denominator)(guarantee)

  type Numerator[F] = F match { case Fraction.WhiteBox[n, ?] => n }
  type Denominator[F] = F match { case Fraction.WhiteBox[?, d] => d }
  type Tupled[F] = F match { case Fraction.WhiteBox[n, d] => Group.FromTuple[(n, d)] }
