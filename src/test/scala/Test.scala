import constraints.{*, given}
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(dividend: Int, divisor: Int)(
    guarantee: Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
  ): Int = dividend / divisor

  // trust example
  {
    val dividend, divisor = Random.between(3, 8)
    // we trust the operands to be between 3 and 8 which meet the constraints
    divide(dividend, divisor)(Guarantee.trust)
  }

  // runtime check example
  {
    val dividend, divisor = Random.nextInt()
    // type alias for brevity
    type Divisible = divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)
    def fallback(guarantee: Guarantee[Not[Divisible]]) = 0
    // we check the operands at runtime in case the random numbers violate the constraints
    Guarantee.testAtRuntime[Divisible] match
      case Right(guarantee: Guarantee[Divisible]) => divide(dividend, divisor)(guarantee)
      case Left(guarantee: Guarantee[Not[Divisible]]) => fallback(guarantee)
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val dividend = Random.nextInt()
    divide(dividend, divisor = 4)(Guarantee.verifyAtCompileTime) // compiles since a positive divisor meets sufficient constraints
  }

  // can provide just a sufficient part of the whole constraint options
  {
    val dividend: Int = Random.nextInt()
    val divisor: 4 = valueOf
    divide(dividend, divisor)(Guarantee.verifyAtCompileTime[divisor.type !== 0 and divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
  }

  // constraint equivalence/satisfaction examples
  {
    type A
    type B
    summon[Guarantee[Not[Not[A]]] =:= Guarantee[A]]
    summon[Guarantee[ForAll[(A, B), [X] =>> X !== 5]] <:< Guarantee[B !== 5]]
    summon[Guarantee[Not[A xor B]] <:< Guarantee[(A and Not[B]) implies Not[A or B]]]
    summon[Guarantee[ForAll[(A, B), [_] =>> true]] =:= Guarantee[Not[Exists[(A, B), [_] =>> false]]]]
  }

  // refinement example
  {
    type NonZero[V] = V !== 0
    def divide(dividend: Int, divisor: Int Constrained NonZero)(
      guarantee: Guarantee[dividend.type !== Int.MinValue.type or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Constrained[NonZero](1)(Guarantee.verifyAtCompileTime) // compiles since 1 != 0
    divide(dividend, divisor)(Guarantee.verifyAtCompileTime) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics = Random.alphanumeric.take(9)

    type Alphanumeric[C]
    alphanumerics.map(Constrained[Alphanumeric](_)(Guarantee.trust)): LazyList[Char Constrained Alphanumeric]

    type Letter[C]
    given [C <: Char: ValueOf]: Compute.Predicate[Letter[C]] = Compute(valueOf[C].isLetter)
    alphanumerics.partitionMap(Constrained.runtimeCheck[Letter](_)): (LazyList[Char Constrained Inverse[Letter]], LazyList[Char Constrained Letter])
  }

  // dependent constraints on collections examples
  {
    Guarantee.verifyAtCompileTime[Unique[(1, 2, 3)]]
    Guarantee.verifyAtCompileTime[Not[Unique[(1, 2, 2)]]]
    Guarantee.verifyAtCompileTime[Unique["abc"]]
    Guarantee.verifyAtCompileTime[Not[Unique["abb"]]]

    val list: LazyList[Char] = Random.alphanumeric.take(3)
    if summon[Compute[Unique[list.type]]].compute
      then println(s"$list has no duplicate values")
      else println(s"$list has duplicate values")

    val a: 1 = 1
    val b: 2 = valueOf
    type Grouped = (a.type, b.type, 3)
    val group: Grouped = valueOf
    type DoubleCheckUniqueness = Unique[group.type] and Unique[(a.type, b.type, 3)]
    Guarantee.verifyAtCompileTime[DoubleCheckUniqueness]
  }

  // joining trust example
  {
    val a = Guarantee.verifyAtCompileTime[1 !== 2]
    val b = Guarantee.verifyAtCompileTime[3 !== 4]
    a and b and Guarantee.verifyAtCompileTime[5 !== 6] and Guarantee.verifyAtCompileTime[7 !== 8]: Guarantee[1 !== 2 and 5 !== 6 and 7 !== 8 and 3 !== 4]
  }

  // corollary example
  {
    def flip[A, B](guarantee: Guarantee[A !== B]): Guarantee[B !== A] = Guarantee.trust
    flip(Guarantee.verifyAtCompileTime[1 !== 2]): Guarantee[2 !== 1]
  }

  // non-primitive (lossless value representation) example
  {
    val numerator: 1 = valueOf
    val denominator: 2 = valueOf
    val fraction = Fraction(numerator, denominator)(Guarantee.verifyAtCompileTime)
    divide(fraction.numerator, fraction.denominator)(Guarantee.verifyAtCompileTime)

    type NonOverflowingOnDivide[F] = Fraction.Numerator[F] !== Int.MinValue.type or Fraction.Denominator[F] !== -1
    Constrained[NonOverflowingOnDivide](fraction)(Guarantee.verifyAtCompileTime)

    val fraction2 = Fraction(1, 3)(Guarantee.verifyAtCompileTime)
    Inlinable.builtin[Fraction.Tupled[fraction.type]].reduce: Some[Fraction.Tupled[fraction.type]]
    Guarantee.verifyAtCompileTime[(1 *: EmptyTuple) === (1 *: EmptyTuple)]
    Guarantee.verifyAtCompileTime[Fraction.Tupled[fraction.type] !== Fraction.Tupled[fraction2.type]]

    val fraction3: Fraction = Fraction(7, Random.between(8, 9))(Guarantee.trust)
    Fraction(6, fraction3.denominator)(fraction3.nonZeroDenominator)
    divide(6, fraction3.denominator)(fraction3.nonZeroDenominator and Guarantee.verifyAtCompileTime)

    type DealiasTest = Singleton & 4 & Int & Singleton & Int & 4 & Int & Int & Singleton
    Guarantee.verifyAtCompileTime[Fraction.Tupled[Fraction.WhiteBox[1, 3]] !== (1, DealiasTest)]
  }

  // Type class and bounds interplay
  {
    val minimum = Percentage(0)(Guarantee.verifyAtCompileTime)
    val maximum = Percentage(1)(Guarantee.verifyAtCompileTime)
    val average = Percentage(.5)(Guarantee.verifyAtCompileTime)
  }

  // Compile time API helpers
  {
    val minimum = Percentage.compileTimeCheck(0)
    val maximum = Constrained[Percentage.Constraint](1d)(Guarantee.verifyAtCompileTime)
    val myGrade: 'B' = valueOf
    val passing: Guarantee[Grade.Passing[myGrade.type]] = Guarantee.verifyAtCompileTime
    import Grade.*
    val widened: Char Constrained Grade = Constrained(myGrade)(passing.toGrade)
    val failing: Char Constrained Grade.Failing = Constrained('F')(Guarantee.verifyAtCompileTime)
  }

  {
    val low: 1 = valueOf
    val high: 2 = valueOf
    Guarantee.verifyAtCompileTime[low.type AtMost high.type]

    type Low = 1
    type High = 2
    Guarantee.verifyAtCompileTime[Low AtMost High]
  }

  // constraint that uses non-boolean expressions in the computation
  {
    def charAt(string: String, index: Int)(
      withinBounds: Guarantee[(index.type AtLeast 0) and (index.type LessThan Length[string.type])]
    ): Char = string.charAt(index)

    charAt("abcde", 3)(Guarantee.verifyAtCompileTime)
  }
