package constraints.rules

import constraints.{Compute, EqualTo, Guarantee, IsNaN, Not}
import org.scalatest.funsuite.AnyFunSuite

class EqualsRelationsTest extends AnyFunSuite {

  test("reflexive"):

    def reflexive[A: Compute : Reflexive]: Guarantee[EqualTo[A, A]] = Reflexive[A]

    assertDoesNotCompile("def missingCompute[A: Reflexive]: Guarantee[EqualTo[A, A]] = EqualTo.reflexive[A]")
    assertDoesNotCompile("def missingReflexive[A: Compute]: Guarantee[EqualTo[A, A]] = EqualTo.reflexive[A]")

  test("Reflexive floating point"):

    def float(float: Float)(using Guarantee[Not[IsNaN[float.type]]]): Reflexive[float.type] = summon
    assertCompiles("def notNaN(float: Float)(using Guarantee[Not[IsNaN[float.type]]]): Reflexive[float.type] = summon")
    assertDoesNotCompile("def possiblyNaN(float: Float): Reflexive[float.type] = summon")

    def double(double: Double)(using Guarantee[Not[IsNaN[double.type]]]): Reflexive[double.type] = summon
    assertCompiles("def notNaN(double: Double)(using Guarantee[Not[IsNaN[double.type]]]): Reflexive[double.type] = summon")
    assertDoesNotCompile("def possiblyNaN(double: Double): Reflexive[double.type] = summon")

  test("symmetric"):

    def symmetric[A, B](guarantee: Guarantee[EqualTo[A, B]])(using Symmetric[A, B]): Guarantee[EqualTo[B, A]] =
      Symmetric(guarantee)

  test("transitive"):

    def transitive[A, B, C](
      guarantee1: Guarantee[EqualTo[A, B]],
      guarantee2: Guarantee[EqualTo[B, C]]
    )(using Transitive[A, B, C]): Guarantee[EqualTo[A, C]] =
      Transitive(guarantee1, guarantee2)

}
