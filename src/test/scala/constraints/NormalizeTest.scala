package constraints

import org.scalatest.funsuite.AnyFunSuite

class NormalizeTest extends AnyFunSuite:

  sealed trait A
  sealed trait B

  test("Nested Not"):
    summon[Normalize[Not[Not[A]]] =:= A]

  test("Negation of conjunction"):
    summon[Normalize[Not[A and B]] =:= (Not[A] | Not[B])]

  test("Negation of disjunction"):
    summon[Normalize[Not[A or B]] =:= (Not[A] & Not[B])]

  test("Negated xor"):
    summon[Normalize[Not[A xor B]] =:= ((Not[A] | B) & (A | Not[B]))]

  test("Negated boolean literals"):
    summon[Normalize[Not[true]] =:= false]
    summon[Normalize[Not[false]] =:= true]
