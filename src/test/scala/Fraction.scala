import constraints.*

import scala.quoted.*

class Fraction(val numerator: Int, val denominator: Int)(
  val nonZeroDenominator: Guarantee[denominator.type !== 0]
):
  def wholeNumberPart(noOverFlow: Guarantee[numerator.type !== Int.MinValue.type or denominator.type !== 1]): Int =
    numerator / denominator

object Fraction:

  type WhiteBox[N, D] = Fraction { val numerator: N; val denominator: D }

  def apply(numerator: Int, denominator: Int)(
    guarantee: Guarantee[denominator.type !== 0]
  ): Fraction.WhiteBox[numerator.type, denominator.type] =
    new Fraction(numerator, denominator)(guarantee).asInstanceOf

  type Numerator[F] = F match { case Fraction.WhiteBox[n, ?] => n }
  type Denominator[F] = F match { case Fraction.WhiteBox[?, d] => d }
  type Tupled[F] = F match { case Fraction.WhiteBox[n, d] => (n, d) }

  given [F <: Fraction.WhiteBox[N, D], N <: Int : ValueOf, D <: Int : ValueOf](using Guarantee[D !== 0]): ValueOf[Fraction.WhiteBox[N, D]] =
    val numerator = valueOf[N]
    val denominator = valueOf[D]
    val fraction = Fraction(numerator, denominator)(Guarantee.trust)
    ValueOf(fraction)

  given wide[F <: Fraction.WhiteBox[N, D], N <: Int : Type, D <: Int : Type](using Quotes): Extractable[F] with
    override def extract: Option[F] =
      for numerator <- Extractable.builtin[N].extract
          denominator <- Extractable.builtin[D].extract
          nonZero <- Guarantee.testAtRuntime[denominator.type !== 0].toOption
      yield Fraction(numerator, denominator)(nonZero).asInstanceOf[F]

  given narrow[F <: Fraction.WhiteBox[N, D], N <: Int & Singleton : Type, D <: Int & Singleton : Type] (using Quotes): Extractable[F] with
    override def extract: Option[F] = wide[F, N, D].extract
