import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(dividend: Int, divisor: Int)(
    divisible: Trust[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
  ): Int = dividend / divisor

  // trust example
  {
    val dividend, divisor = Random.between(3, 8)
    // we trust the operands to be between 3 and 8 which meet the constraints
    divide(dividend, divisor)(Trust.belief)
  }

  // runtime check example
  {
    val dividend, divisor = Random.nextInt()
    // type alias for brevity
    type Divisible = divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)
    // we check the operands at runtime in case the random numbers violate the constraints
    Trust.runtimeCheck[Divisible] match
      case Right(trust: Trust[Divisible]) => divide(dividend, divisor)(trust)
      case Left(_) => println(s"cannot divide($dividend, $divisor)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val dividend = Random.nextInt()
    divide(dividend, divisor = 4)(Trust.compileTimeCheck) // compiles since a positive divisor meets sufficient constraints
  }

  // can provide just a sufficient part of the whole constraint options
  {
    val dividend: Int = Random.nextInt()
    val divisor: 4 = valueOf
    divide(dividend, divisor)(Trust.compileTimeCheck[divisor.type !== 0 and divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
  }

  // constraint equivalence/satisfaction examples
  {
    type A
    type B
    summon[Trust[not[not[A]]] =:= Trust[A]]
    summon[Trust[(A, B) ForAll ([X] =>> X !== 5)] <:< Trust[B !== 5]]
    summon[Trust[not[A xor B]] <:< Trust[(A and not[B]) implies not[A or B]]]
  }

  // refinement example
  {
    type NonZero[V] = V !== 0
    def divide(dividend: Int, divisor: Int Constrained NonZero)(
      noOverflow: Trust[dividend.type !== Int.MinValue.type or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Constrained[NonZero](1)(Trust.compileTimeCheck) // compiles since 1 != 0
    divide(dividend, divisor)(Trust.compileTimeCheck) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics = Random.alphanumeric.take(9)

    trait Alphanumeric[C]
    alphanumerics.map(Constrained[Alphanumeric](_)(Trust.belief)): LazyList[Char Constrained Alphanumeric]

    trait Letter[C]
    given letterRuntimeCheck[C <: Char: ValueOf]: RuntimeCheck[Letter[C]] = RuntimeCheck(valueOf[C].isLetter)
    Constrained.partition(alphanumerics)(c => letterRuntimeCheck[c.type]): (LazyList[Char Constrained Inverse[Letter]], LazyList[Char Constrained Letter])
  }

  // dependent constraints on collections examples
  {
    Trust.compileTimeCheck[Unique[(1, 2, 3)]]
    Trust.compileTimeCheck[not[Unique[(1, 2, 2)]]]
    Trust.compileTimeCheck[Unique["abc"]]
    Trust.compileTimeCheck[not[Unique["abb"]]]

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
    Trust.compileTimeCheck[DoubleCheckUniqueness]
  }

  // joining trust example
  {
    val a = Trust.compileTimeCheck[1 !== 2]
    val b = Trust.compileTimeCheck[3 !== 4]
    a and b and Trust.compileTimeCheck[5 !== 6] and Trust.compileTimeCheck[7 !== 8]: Trust[1 !== 2 and 5 !== 6 and 7 !== 8 and 3 !== 4]
  }

  // corollary example
  {
    def flip[A, B](trust: Trust[A !== B]): Trust[B !== A] = Trust.belief
    flip(Trust.compileTimeCheck[1 !== 2]): Trust[2 !== 1]
  }

  // non-primitive (lossless value representation) example
  {
    val numerator: 1 = valueOf
    val denominator: 2 = valueOf
    val fraction = Fraction(numerator, denominator)(Trust.compileTimeCheck)
    divide(fraction.numerator, fraction.denominator)(Trust.compileTimeCheck)

    type NonOverflowingOnDivide[F <: Fraction.WhiteBox[_ <: Singleton, _ <: Singleton]] = Fraction.Numerator[F] !== Int.MinValue.type or Fraction.Denominator[F] !== -1
    Constrained(fraction)[NonOverflowingOnDivide].apply(Trust.compileTimeCheck)

    val fraction2 = Fraction(1, 3)(Trust.compileTimeCheck)
    Trust.compileTimeCheck[Fraction.Tupled[fraction.type] !== Fraction.Tupled[fraction2.type]]

    val fraction3: Fraction = Fraction(7, Random.between(8, 9))(Trust.belief)
    Fraction(6, fraction3.denominator)(fraction3.nonZeroDenominator)
    divide(6, fraction3.denominator)(fraction3.nonZeroDenominator and Trust.compileTimeCheck)

    type DealiasTest = Singleton & 4 & Int & Singleton & Int & 4 & Int & Int & Singleton
    Trust.compileTimeCheck[Fraction.Tupled[Fraction.WhiteBox[1, 3]] !== ((1, DealiasTest) & Singleton)]

  }
