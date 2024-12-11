import constraints.Guarantee.Impl
import constraints.compile.{*, given}
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
    Guarantee.test[Divisible] match
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
    divide(dividend, divisor)(Guarantee[divisor.type !== 0 and divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
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

    def divide(dividend: Int, divisor: Guaranteed.Refined[Int, NonZero])(
      guarantee: Guarantee[dividend.type !== Int.MinValue.type or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Guaranteed.Refined[NonZero](1)(Guarantee.verifyAtCompileTime) // compiles since 1 != 0
    divide(dividend, divisor)(Guarantee.verifyAtCompileTime) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics = Random.alphanumeric.take(9)

    type Alphanumeric[C]
    alphanumerics.map(Guaranteed.Refined[Alphanumeric](_)(Guarantee.trust)): LazyList[Guaranteed.Refined[Char, Alphanumeric]]

    type Letter[C]

    given[C <: Char : ValueOf]: Compute.Predicate[Letter[C]] = Compute(valueOf[C].isLetter)

    alphanumerics.partitionMap(Guaranteed.Refined.runtimeCheck[Letter](_)): (LazyList[Guaranteed.Refined[Char, Inverse[Letter]]], LazyList[Guaranteed.Refined[Char, Letter]])
  }

  // dependent constraints on collections examples
  {
    Guarantee[Unique[(1, 2, 3)]]
    Guarantee[Not[Unique[(1, 2, 2)]]]
    Guarantee[Unique["abc"]]
    Guarantee[Not[Unique["abb"]]]

    val list: LazyList[Char] = Random.alphanumeric.take(3)
    if summon[Compute[Unique[list.type]]].compute
    then println(s"$list has no duplicate values")
    else println(s"$list has duplicate values")

    val a: 1 = 1
    val b: 2 = valueOf
    type Grouped = (a.type, b.type, 3)
    val group: Grouped = Compute[Grouped]
    type DoubleCheckUniqueness = Unique[group.type] and Unique[(a.type, b.type, 3)]
    Guarantee[DoubleCheckUniqueness]
  }

  // joining trust example
  {
    val a = Guarantee[1 !== 2]
    val b = Guarantee[3 !== 4]
    a and b and Guarantee[5 !== 6] and Guarantee[7 !== 8]: Guarantee[1 !== 2 and 5 !== 6 and 7 !== 8 and 3 !== 4]
  }

  // corollary example
  {
    def flip[A, B](guarantee: Guarantee[A !== B]): Guarantee[B !== A] = Guarantee.trust

    flip(Guarantee[1 !== 2]): Guarantee[2 !== 1]
  }

  // non-primitive (lossless value representation) example
  {
    val numerator: 1 = valueOf
    val denominator: 2 = valueOf
    val fraction = Fraction(numerator, denominator)(Guarantee.verifyAtCompileTime)
    divide(fraction.numerator, fraction.denominator)(Guarantee.verifyAtCompileTime)

    val fraction2 = Fraction(1, 3)(Guarantee.verifyAtCompileTime)
    Guarantee[(1 *: EmptyTuple) === (1 *: EmptyTuple)]

    val fraction3: Fraction = Fraction(7, Random.between(8, 9))(Guarantee.trust)
    Fraction(6, fraction3.denominator)(fraction3.nonZeroDenominator)
    divide(6, fraction3.denominator)(fraction3.nonZeroDenominator and Guarantee.verifyAtCompileTime)
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
    val maximum =
      new Guaranteed.Type[1d]:
        override def guarantee: Guarantee[Percentage.Constraint[value.type]] = Guarantee.verifyAtCompileTime
    val myGrade: 'B' = valueOf
    val passing: Guarantee[Grade.Passing[myGrade.type]] = Guarantee.verifyAtCompileTime
    import Grade.*
    val widened: Guaranteed.Refined[Char, Grade] = Guaranteed.Refined(myGrade)(passing.toGrade)
    val failing: Guaranteed.Refined[Char, Grade.Failing] = Guaranteed.Refined('F')(Guarantee.verifyAtCompileTime)
  }

  {
    val low: 1 = valueOf
    val high: 2 = valueOf
    Guarantee[low.type AtMost high.type]

    type Low = 1
    type High = 2
    Guarantee[Low AtMost High]
  }

  // constraint that uses non-boolean expressions in the computation
  {
    def charAt(string: String, index: Int)(
      withinBounds: Guarantee[(index.type AtLeast 0) and (index.type LT Length[string.type])]
    ): Char = string.charAt(index)

    charAt("abcde", 3)(Guarantee.verifyAtCompileTime)
  }

  {
    def divide(dividend: Int, divisor: Int)(using
      Guarantee[divisor.type !== 0],
      Guarantee[dividend.type !== Int.MinValue.type or divisor.type !== -1]
    ): Int = dividend / divisor

    val dividend, divisor = Random.nextInt()

    summon[
      Guarantee[divisor.type === 0 or (dividend.type === Int.MinValue.type and divisor.type === -1)]
        =:=
      Guarantee[Not[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]]
    ]
    summon[
      Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
        <:<
      Guarantee[dividend.type !== Int.MinValue.type or divisor.type !== -1]
    ]
    summon[
      Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]
        <:<
        Guarantee[divisor.type !== 0]
    ]
    Guarantee.test[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)] match
      case Left(given Guarantee[divisor.type === 0 or (dividend.type === Int.MinValue.type and divisor.type === -1)]) =>
        // divide(dividend, divisor)
      case Right(given Guarantee[divisor.type !== 0 and (dividend.type !== Int.MinValue.type or divisor.type !== -1)]) =>
        divide(dividend, divisor)
  }
