import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(dividend: Int, divisor: Int)(
    using Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
  ): Int = dividend / divisor

  // trust example
  {
    val dividend, divisor = Random.between(3, 8)
    // we trust the operands to be between 3 and 8 which meet the constraints
    divide(dividend, divisor)(using Guarantee.trust)
  }

  // runtime check example
  {
    val dividend, divisor = Random.nextInt()
    // type alias for brevity
    type Divisible = divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)
    // we check the operands at runtime in case the random numbers violate the constraints
    Guarantee.runtimeCheck[Divisible] match
      case Right(given Guarantee[Divisible]) => divide(dividend, divisor)
      case Left(_) => println(s"cannot divide($dividend, $divisor)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val dividend = Random.nextInt()
    divide(dividend, divisor = 4) // compiles since a positive divisor meets sufficient constraints
  }

  // can provide just a sufficient part of the whole constraint options
  {
    val dividend: Int = Random.nextInt()
    val divisor: 4 = valueOf
    divide(dividend, divisor)(using Guarantee.compileTimeCheck[divisor.type !== 0 and divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
  }

  // constraint equivalence/satisfaction examples
  {
    type A
    type B
    summon[Guarantee[not[not[A]]] =:= Guarantee[A]]
    summon[Guarantee[(A, B) ForAll ([X] =>> X !== 5)] <:< Guarantee[B !== 5]]
    summon[Guarantee[not[A xor B]] <:< Guarantee[(A and not[B]) implies not[A or B]]]
  }

  // refinement example
  {
    type NonZero[V] = V !== 0
    def divide(dividend: Int, divisor: Int Constrained NonZero)(
      using Guarantee[dividend.type !== Int.MinValue.type or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Constrained[NonZero](1) // compiles since 1 != 0
    divide(dividend, divisor) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics = Random.alphanumeric.take(9)

    trait Alphanumeric[C]
    alphanumerics.map(Constrained[Alphanumeric](_)(using Guarantee.trust)): LazyList[Char Constrained Alphanumeric]

    trait Letter[C]
    given letterRuntimeCheck[C <: Char: ValueOf]: RuntimeCheck[Letter[C]] = RuntimeCheck(valueOf[C].isLetter)
    Constrained.partition(alphanumerics)(c => letterRuntimeCheck[c.type]): (LazyList[Char Constrained Inverse[Letter]], LazyList[Char Constrained Letter])
  }

  // dependent constraints on collections examples
  {
    Guarantee.compileTimeCheck[Unique[(1, 2, 3)]]
    Guarantee.compileTimeCheck[not[Unique[(1, 2, 2)]]]
    Guarantee.compileTimeCheck[Unique["abc"]]
    Guarantee.compileTimeCheck[not[Unique["abb"]]]

    val list: LazyList[Char] = Random.alphanumeric.take(3)
    if summon[RuntimeCheck[Unique[list.type]]].succeeded
      then println(s"$list has no duplicate values")
      else println(s"$list has duplicate values")

    val a: 1 = 1
    val b: 2 = valueOf
    type Tupled = (a.type, b.type, 3)
    import constraints.nonEmptyTupleValueOf // not sure why the standard library doesn't provide this...
    val tuple: Tupled = valueOf
    type DoubleCheckUniqueness = Unique[tuple.type] and Unique[(a.type, b.type, 3)]
    Guarantee.compileTimeCheck[DoubleCheckUniqueness]
  }

  // joining trust example
  {
    val a = Guarantee.compileTimeCheck[1 !== 2]
    val b = Guarantee.compileTimeCheck[3 !== 4]
    a and b and Guarantee.compileTimeCheck[5 !== 6] and Guarantee.compileTimeCheck[7 !== 8]: Guarantee[1 !== 2 and 5 !== 6 and 7 !== 8 and 3 !== 4]
  }

  // corollary example
  {
    def flip[A, B](guarantee: Guarantee[A !== B]): Guarantee[B !== A] = Guarantee.trust
    flip(Guarantee.compileTimeCheck[1 !== 2]): Guarantee[2 !== 1]
  }

  // non-primitive (lossless value representation) example
  {
    val numerator: 1 = valueOf
    val denominator: 2 = valueOf
    val fraction = Fraction(numerator, denominator)
    divide(fraction.numerator, fraction.denominator)

    type NonOverflowingOnDivide[F] = Fraction.Numerator[F] !== Int.MinValue.type or Fraction.Denominator[F] !== -1
    Constrained[NonOverflowingOnDivide](fraction)

    val fraction2 = Fraction(1, 3)
    Guarantee.compileTimeCheck[Fraction.Tupled[fraction.type] !== Fraction.Tupled[fraction2.type]]

    val fraction3: Fraction = Fraction(7, Random.between(8, 9))(using Guarantee.trust)
    Fraction(6, fraction3.denominator)(using fraction3.nonZeroDenominator)
    divide(6, fraction3.denominator)(using fraction3.nonZeroDenominator and Guarantee.compileTimeCheck)

    type DealiasTest = Singleton & 4 & Int & Singleton & Int & 4 & Int & Int & Singleton
    Guarantee.compileTimeCheck[Fraction.Tupled[Fraction.WhiteBox[1, 3]] !== ((1, DealiasTest) & Singleton)]

  }
