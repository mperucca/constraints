import constraints.*
import constraints.compile.*

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

  given [N <: Int : ValueOf, D <: Int](using d: ValueOf[D])(using g: Guarantee[d.value.type !== 0]): ValueOf[Fraction.WhiteBox[N, D]] =
    ValueOf(Fraction(valueOf[N], d.value)(g))

  given wide[F <: Fraction.WhiteBox[N, D], N <: Int : Type, D <: Int : Type]: FromType[F] with
    override def extract(using Quotes): Option[F] =
      for numerator <- FromType[N]
          denominator <- FromType[D]
          nonZero <- Guarantee.testAtRuntime[denominator.type !== 0].toOption
      yield Fraction(numerator, denominator)(nonZero).asInstanceOf[F]

  given narrow[F <: Fraction.WhiteBox[N, D], N <: Int & Singleton : Type, D <: Int & Singleton : Type]: FromType[F] with
    override def extract(using Quotes): Option[F] = wide[F, N, D].extract

  given toExpr[F <: Fraction: Type]: ToExpr[F] with
    override def apply(fraction: F)(using Quotes): Expr[F] =
      val numerator = Expr(fraction.numerator)
      val denominator = Expr(fraction.denominator)
      '{ Fraction($numerator, $denominator)(Guarantee.trust).asInstanceOf[F] }

  given toType[F <: Fraction]: ToType[F] with
    override def apply(fraction: F)(using Quotes): Type[? <: F] =
      import quoted.quotes.reflect.*
      val numeratorType = ToType(fraction.numerator)
      val denominatorType = ToType(fraction.denominator)
      (numeratorType, denominatorType) match
        case ('[n], '[d]) =>
          Type.of[Fraction.WhiteBox[n, d]].asInstanceOf[Type[? <: F]]
