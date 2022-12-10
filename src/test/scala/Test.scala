import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(dividend: Int, divisor: Int)(
    using Proof[divisor.type !== 0 And (dividend.type !== Int.MinValue.type Or divisor.type !== -1)]
  ): Int = dividend / divisor

  // trust example
  {
    val dividend, divisor = Random.between(3, 8)
    // we trust the operands to be between 3 and 8 which meet the proof constraints
    divide(dividend, divisor)(using Proof.unchecked)
  }

  // runtime check example
  {
    val dividend, divisor = Random.nextInt()
    // type alias for brevity
    type Divisible = divisor.type !== 0 And (dividend.type !== Int.MinValue.type Or divisor.type !== -1)
    // we check the operands at runtime in case the random numbers violate the constraints
    Proof.runtimeCheck[Divisible] match
      case Right(given Proof[Divisible]) => divide(dividend, divisor) // compiles since a proof is now in scope
      case Left(_) => println(s"cannot divide($dividend, $divisor)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val dividend = Random.nextInt()
    divide(dividend, divisor = 4) // compiles since a positive divisor meets sufficient constraints
  }

  // can provide just a sufficient part of the whole proof options
  {
    val dividend: Int = Random.nextInt()
    val divisor: 4 = valueOf
    divide(dividend, divisor)(using Proof[divisor.type !== 0 And divisor.type !== -1]) // compiles since knowing the divisor isn't 0 nor -1 meets sufficient constraints
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
    def divide(dividend: Int, divisor: Int Refinement NonZero)(
      using Proof[dividend.type !== Int.MinValue.type Or divisor.value.type !== -1]
    ): Int = dividend / divisor.value

    val dividend = Random.nextInt()
    val divisor = Refinement[NonZero](1) // compiles since 1 != 0
    divide(dividend, divisor) // compiles since refinement on divisor exposes value as a literal 1 type
  }

  // independent constraints on collections example
  {
    val alphanumerics: Iterable[Char] = Random.alphanumeric.take(9)

    trait Alphanumeric[C]
    alphanumerics.map(Refinement(_)(using Proof.unchecked)): Iterable[Char Refinement Alphanumeric]

    trait Letter[C]
    given letterRuntimeCheck[C <: Char: ValueOf]: RuntimeCheck[Letter[C]] = RuntimeCheck(valueOf[C].isLetter)
    RuntimeCheck.all(alphanumerics)(c => letterRuntimeCheck[c.type]): (Iterable[Char Refinement Inverse[Letter]], Iterable[Char Refinement Letter])
  }

  // dependent constraints on collections examples
  {
    Proof.compileTimeCheck[Unique[(1, 2, 3)]]
    Proof.compileTimeCheck[Not[Unique[(1, 2, 2)]]]
    Proof.compileTimeCheck[Unique["abc"]]
    Proof.compileTimeCheck[Not[Unique["abb"]]]

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
    Proof.compileTimeCheck[DoubleCheckUniqueness]
  }

  // joining proofs example
  {
    val a = Proof[1 !== 2]
    val b = Proof[3 !== 4]
    a and b: Proof[1 !== 2 And 3 !== 4]
  }

  // corollaries example
  {
    given [A <: Int, B <: Int]: Corollary[A !== B, B !== A] = Corollary
    val proof = Proof[1 !== 2]
    proof.corollaries: Proof[2 !== 1]
  }

