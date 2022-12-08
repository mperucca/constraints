import constraints.*
import scala.util.Random

@main def test(): Unit =

  // safer divide method
  def divide(a: Int, b: Int)(
    proof: Proof[b.type !== 0 And (a.type !== Int.MinValue.type Or b.type !== -1)]
  ): Int = a / b

  // trust example
  {
    val a, b = Random.between(3, 8)
    divide(a, b)(
      proof = Proof.trust[b.type !== 0 And (a.type !== Int.MinValue.type Or b.type !== -1)]
    )
  }

  type Divisible[A <: Int, B <: Int] = B !== 0 And (A !== Int.MinValue.type Or B !== -1)

  // runtime check example
  {
    val a, b = Random.nextInt()
    Proof.attempt[Divisible[a.type, b.type]] match
      case Some(proof: Divisible[a.type, b.type]) => divide(a, b)(proof)
      case None => println(s"cannot divide($a, $b)")
  }

  // can still prove with unknown if simplification makes knowledge unnecessary
  {
    val a = Random.nextInt()
    divide(a, 4)(Proof[Divisible[a.type, 4]])
  }

  // can provide just a sufficient part of the whole proof options
  {
    val b: 5 = valueOf
    val proof = Proof[b.type !== 0 And b.type !== -1]
    divide(Random.nextInt(), b)(proof)
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
      val proof: Proof[value.type !== 0]
    )
    def divide(a: Int, b: NonZero[Int])(
      noOverflowProof: Proof[
        a.type !== Int.MinValue.type Or b.value.type !== -1
      ]
    ): Int = a / b.value

    val a = Random.nextInt()
    val i: 1 = valueOf
    val b = NonZero[i.type](i)(Proof[i.type !== 0])
    divide(a, b)(noOverflowProof = Proof[b.value.type !== -1])
  }

  // generic refinement example
  {
    type NonZero = [V] =>> V !== 0
    def divide(a: Int, b: Refinement[Int, NonZero])(
      noOverflowProof: Proof[
        a.type !== Int.MinValue.type Or b.value.type !== -1
      ]
    ): Int = a / b.value

    val a = Random.nextInt()
    val b = Refinement.prove[NonZero](1)
    divide(a, b)(noOverflowProof = Proof[b.value.type !== -1])
  }

  // independent constraints on collections example
  {
    val characters: Iterable[Char] = Random.alphanumeric.take(9)
    trait Alphanumeric[C]
    val alphanumerics: Iterable[Refinement[Char, Alphanumeric]] = characters.map(Refinement.trust)
  }

  // dependent constraints on collections examples
  {
    Proof[Unique[(1, 2, 3)]]
    Proof[Not[Unique[(1, 2, 2)]]]
    Proof[Unique["abc"]]
    Proof[Not[Unique["abb"]]]

    val list: LazyList[Char] = Random.alphanumeric.take(3)
    Proof.attempt[Unique[list.type]] match
      case Some(proof: Unique[list.type]) => println(s"$list has no duplicate values")
      case None => println(s"$list has duplicate values")

    val a: 1 = 1
    val b: 2 = valueOf
    type Tupled = (a.type, b.type, 3)
    import constraints.nonEmptyTupleValueOf
    val tuple: Tupled = valueOf
    type DoubleCheckUniqueness = Unique[tuple.type] And Unique[(a.type, b.type, 3)]
    Proof[DoubleCheckUniqueness]
  }



