import constraints.*

sealed trait Fraction:
  val numerator: Int
  val denominator: Int
  val nonZeroDenominator: Proof[denominator.type !== 0]

class FractionExposed[+N <: Int, +D <: Int](val numerator: N, val denominator: D)(using val nonZeroDenominator: Proof[denominator.type !== 0]) extends Fraction

type Numerator[F <: Fraction] = F match
  case FractionExposed[n, _] => n
type Denominator[F <: Fraction] = F match
  case FractionExposed[_, d] => d

object Fraction:
  def apply(numerator: Int, denominator: Int)(using Proof[denominator.type !== 0]): FractionExposed[numerator.type, denominator.type] =
    new FractionExposed(numerator, denominator)
