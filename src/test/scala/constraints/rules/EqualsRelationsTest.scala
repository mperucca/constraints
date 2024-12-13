package constraints.rules

import constraints.{Compute, ==, Guarantee, IsNaN, Not}
import org.scalatest.funsuite.AnyFunSuite

class EqualsRelationsTest extends AnyFunSuite {

  test("reflexive"):

    def reflexive[A: Compute : Reflexive]: Guarantee[A == A] = Reflexive[A]

    assertDoesNotCompile("def missingCompute[A: Reflexive]: Guarantee[A == A] = reflexive[A]")
    assertDoesNotCompile("def missingReflexive[A: Compute]: Guarantee[A == A] = reflexive[A]")

  test("Reflexive floating point"):

    def float(float: Float)(using Guarantee[Not[IsNaN[float.type]]]): Reflexive[float.type] = summon
    assertCompiles("def notNaN(float: Float)(using Guarantee[Not[IsNaN[float.type]]]): Reflexive[float.type] = summon")
    assertDoesNotCompile("def possiblyNaN(float: Float): Reflexive[float.type] = summon")

    def double(double: Double)(using Guarantee[Not[IsNaN[double.type]]]): Reflexive[double.type] = summon
    assertCompiles("def notNaN(double: Double)(using Guarantee[Not[IsNaN[double.type]]]): Reflexive[double.type] = summon")
    assertDoesNotCompile("def possiblyNaN(double: Double): Reflexive[double.type] = summon")

  test("symmetric"):

    def symmetric[A, B](guarantee: Guarantee[A == B])(using Symmetric[A, B]): Guarantee[B == A] =
      Symmetric(guarantee)

  test("transitive"):

    def transitive[A, B, C](
      guarantee1: Guarantee[A == B],
      guarantee2: Guarantee[B == C]
    )(using Transitive[A, B, C]): Guarantee[A == C] =
      Transitive(guarantee1, guarantee2)

}
