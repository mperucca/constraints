import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(a: Int, b: Int)(
    using Proof[b.type !== 0 And (a.type !== Int.MinValue.type Or b.type !== -1)]
  ): Int = a / b

  // trust example
  {
    val a, b = Random.between(3, 8)
    divide(a, b)(using Proof.unchecked)
  }

  type Divisible[A <: Int, B <: Int] = B !== 0 And (A !== Int.MinValue.type Or B !== -1)

  // runtime check example
  {
    val a, b = Random.nextInt()
    Proof.runtimeCheck[Divisible[a.type, b.type]] match
      case Some(given Proof[Divisible[a.type, b.type]]) => divide(a, b)
      case None => println(s"cannot divide($a, $b)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val a = Random.nextInt()
    divide(a, 4)
  }

  // can provide just a sufficient part of the whole proof options
  {
    val b = Random.between(1, 6)
    given positiveProof: Proof[b.type !== 0 And b.type !== -1] = Proof.unchecked
    divide(Random.nextInt(), b)
  }

  // proof equivalence/satisfaction examples
  {
    type A
    type B
    summon[Proof[Not[Not[A]]] =:= Proof[A]]
    summon[
      Proof[ForAll[(A, B), [X] =>> X !== 5]] <:< Proof[B !== 5]
    ]
    summon[
      Proof[Not[A Xor B]]
        <:<
        Proof[(A And Not[B]) Implies Not[A Or B]]
    ]
  }

  // specific refinement example
  {
    class NonZero[+I](val value: I)(
      using proof: Proof[value.type !== 0]
    )
    def divide(a: Int, b: NonZero[Int])(
      using Proof[a.type !== Int.MinValue.type Or b.value.type !== -1]
    ): Int = a / b.value

    val a = Random.nextInt()
    val i: 1 = valueOf
    val b = NonZero[i.type](i)
    divide(a, b)
  }

  // generic refinement example
  {
    type NonZero = [V] =>> V !== 0
    def divide(a: Int, b: Int Refinement NonZero)(
      using Proof[a.type !== Int.MinValue.type Or b.value.type !== -1]
    ): Int = a / b.value

    val a = Random.nextInt()
    val b = Refinement[NonZero](1)
    divide(a, b)
  }

  // independent constraints on collections example
  {
    val alphanumerics: Iterable[Char] = Random.alphanumeric.take(9)
    trait Alphanumeric[C]
    alphanumerics.map(Refinement.unchecked): Iterable[Char Refinement Alphanumeric]
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
    import constraints.nonEmptyTupleValueOf
    val tuple: Tupled = valueOf
    type DoubleCheckUniqueness = Unique[tuple.type] And Unique[(a.type, b.type, 3)]
    Proof.compileTimeCheck[DoubleCheckUniqueness]
  }

  // joining proofs example
  {
    val a = Proof[1 !== 2]
    val b = Proof[3 !== 4]
    Proof.and(a, b): Proof[1 !== 2 And 3 !== 4]
  }

  // corollaries example
  {
    given [A <: Int, B <: Int]: Corollary[A !== B, B !== A] = Corollary
    Proof[1 !== 2].corollaries: Proof[2 !== 1]
  }

