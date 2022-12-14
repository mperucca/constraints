import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(dividend: Int, divisor: Int)(
    divisibleProof: Proof[divisor.type !== 0 And (dividend.type !== Int.MinValue.type Or divisor.type !== -1)]
  ): Int = dividend / divisor

  // trust example
  {
    val dividend, divisor = Random.between(3, 8)
    // we trust the operands to be between 3 and 8 which meet the proof constraints
    divide(dividend, divisor)(Proof.unchecked)
  }

  // runtime check example
  {
    val dividend, divisor = Random.nextInt()
    // type alias for brevity
    type Divisible = divisor.type !== 0 And (dividend.type !== Int.MinValue.type Or divisor.type !== -1)
    // we check the operands at runtime in case the random numbers violate the constraints
    Proof.checkAtRuntime[Divisible] match
      case Right(given Proof[Divisible]) => divide(dividend, divisor) // compiles since a proof is now in scope
      case Left(_) => println(s"cannot divide($dividend, $divisor)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val dividend = Random.nextInt()
    divide(dividend, divisor = 4)(Proof.checkAtCompileTime) // compiles since a positive divisor meets sufficient constraints
  }

  // can provide just a sufficient part of the whole proof options
  {
    val dividend: Int = Random.nextInt()
    val divisor: 4 = valueOf
    divide(dividend, divisor)(Proof.checkAtCompileTime[divisor.type !== 0 And divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
  }

  // proof equivalence/satisfaction examples
  {
    type A
    type B
    summon[Proof[Not[Not[A]]] =:= Proof[A]]
    summon[Proof[(A, B) ForAll ([X] =>> X !== 5)] <:< Proof[B !== 5]]
    summon[Proof[Not[A Xor B]] <:< Proof[(A And Not[B]) Implies Not[A Or B]]]
  }

  // refinement example
  {
    type NonZero = [V] =>> V !== 0
    def divide(dividend: Int, divisor: Int Constrained NonZero)(
      noOverflowProof: Proof[dividend.type !== Int.MinValue.type Or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Constrained[NonZero](1)(Proof.checkAtCompileTime) // compiles since 1 != 0
    divide(dividend, divisor)(Proof.checkAtCompileTime) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics = Random.alphanumeric.take(9)

    trait Alphanumeric[C]
    alphanumerics.map(Constrained[Alphanumeric](_)(Proof.unchecked)): LazyList[Char Constrained Alphanumeric]

    trait Letter[C]
    given letterRuntimeCheck[C <: Char: ValueOf]: RuntimeCheck[Letter[C]] = RuntimeCheck(valueOf[C].isLetter)
    Constrained.partition(alphanumerics)(c => letterRuntimeCheck[c.type]): (LazyList[Char Constrained Inverse[Letter]], LazyList[Char Constrained Letter])
  }

  // dependent constraints on collections examples
  {
    Proof.checkAtCompileTime[Unique[(1, 2, 3)]]
    Proof.checkAtCompileTime[Not[Unique[(1, 2, 2)]]]
    Proof.checkAtCompileTime[Unique["abc"]]
    Proof.checkAtCompileTime[Not[Unique["abb"]]]

    val list: LazyList[Char] = Random.alphanumeric.take(3)
    if summon[RuntimeCheck[Unique[list.type]]].succeeds
      then println(s"$list has no duplicate values")
      else println(s"$list has duplicate values")

    val a: 1 = 1
    val b: 2 = valueOf
    type Tupled = (a.type, b.type, 3)
    import constraints.nonEmptyTupleValueOf // not sure why the standard library doesn't provide this...
    val tuple: Tupled = valueOf
    type DoubleCheckUniqueness = Unique[tuple.type] And Unique[(a.type, b.type, 3)]
    Proof.checkAtCompileTime[DoubleCheckUniqueness]
  }

  // joining proofs example
  {
    val a = Proof.checkAtCompileTime[1 !== 2]
    val b = Proof.checkAtCompileTime[3 !== 4]
    a and b and Proof.checkAtCompileTime[5 !== 6] and Proof.checkAtCompileTime[7 !== 8]: Proof[1 !== 2 And 5 !== 6 And 7 !== 8 And 3 !== 4]
  }

  // corollary example
  {
    def flip[A, B](proof: Proof[A !== B]): Proof[B !== A] = Proof.unchecked
    flip(Proof.checkAtCompileTime[1 !== 2]): Proof[2 !== 1]
  }

  // non-primitive (lossless value representation) example
  {
    val numerator: 1 = valueOf
    val denominator: 2 = valueOf
    val fraction = Fraction(numerator, denominator)(Proof.checkAtCompileTime)
    divide(fraction.numerator, fraction.denominator)(Proof.checkAtCompileTime)

    type NonOverflowingOnDivide[F <: Fraction] = Fraction.Numerator[F] !== Int.MinValue.type Or Fraction.Denominator[F] !== -1
    Constrained(fraction)[NonOverflowingOnDivide]

    val fraction2 = Fraction(1, 3)(Proof.checkAtCompileTime)
    Proof.checkAtCompileTime[fraction.Tupled !== Fraction.Tupled[fraction2.type]]

    val fraction3: Fraction = Fraction(7, Random.between(8, 9))(Proof.unchecked)
    Fraction(6, fraction3.denominator)(fraction3.nonZeroDenominatorProof)
    divide(6, fraction3.denominator)(fraction3.nonZeroDenominatorProof and Proof.checkAtCompileTime)
  }
