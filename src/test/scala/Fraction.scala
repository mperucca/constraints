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

  given [F <: Fraction.WhiteBox[N, D], N <: Int : ValueOf, D <: Int](using d: ValueOf[D])(using Guarantee[d.value.type !== 0]): ValueOf[Fraction.WhiteBox[N, D]] =
    ValueOf(Fraction(valueOf[N], d.value)(summon))

  given wide[F <: Fraction.WhiteBox[N, D], N <: Int : Type, D <: Int : Type]: FromType[F] with
    override def extract(using Quotes): Option[F] =
      for numerator <- FromType.builtin[N].extract
          denominator <- FromType.builtin[D].extract
          nonZero <- Guarantee.testAtRuntime[denominator.type !== 0].toOption
      yield Fraction(numerator, denominator)(nonZero).asInstanceOf[F]

  given narrow[F <: Fraction.WhiteBox[N, D], N <: Int & Singleton : Type, D <: Int & Singleton : Type]: FromType[F] with
    override def extract(using Quotes): Option[F] = wide[F, N, D].extract

  given literable[F <: Fraction: Type]: Literable[F] with
    override def toLiteral(fraction: F)(using Quotes): Expr[F] =
      val numerator = Expr(fraction.numerator)
      val denominator = Expr(fraction.denominator)
      '{Fraction($numerator, $denominator)(Guarantee.trust).asInstanceOf[F]}

  given refinable: Refinable[Fraction] with
    override def refine(fraction: Fraction)(using Quotes): quoted.quotes.reflect.Refinement =
      import quoted.quotes.reflect.*
      val numeratorType = Primitive.toConstantType(fraction.numerator)
      val denominatorType = Primitive.toConstantType(fraction.denominator)
      Refinement(Refinement(TypeRepr.of[Fraction], "numerator", numeratorType), "denominator", denominatorType)
