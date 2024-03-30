package constraints

import constraints.EqualTo.Reflexive
import org.scalatest.funsuite.AnyFunSuite

class EqualToTest extends AnyFunSuite {

  test("NaN"):
    ComputeTest[EqualTo[Double.NaN.type, Double.NaN.type]](false)
    ComputeTest[Not[EqualTo[Double.NaN.type, Double.NaN.type]]](true)
    ComputeTest[Not[Not[EqualTo[Double.NaN.type, Double.NaN.type]]]](false)

  test("reflexive"):
    def reflexive[A: Compute: Reflexive]: Guarantee[EqualTo[A, A]] = EqualTo.reflexive[A]
    assertDoesNotCompile("def missingCompute[A: Reflexive]: Guarantee[EqualTo[A, A]] = EqualTo.reflexive[A]")
    assertDoesNotCompile("def missingReflexive[A: Compute]: Guarantee[EqualTo[A, A]] = EqualTo.reflexive[A]")

  test("Reflexive floating point"):
    def float(float: Float)(using Guarantee[Not[IsNaN[float.type]]]): Reflexive[float.type] = summon
    def double(double: Double)(using Guarantee[Not[IsNaN[double.type]]]): Reflexive[double.type] = summon

  test("symmetric"):
    def symmetric[A, B](guarantee: Guarantee[EqualTo[A, B]]): Guarantee[EqualTo[B, A]] = guarantee.symmetric

  test("transitive"):
    def transitive[A, B, C](
      guarantee1: Guarantee[EqualTo[A, B]],
      guarantee2: Guarantee[EqualTo[B, C]]
    ): Guarantee[EqualTo[A, C]] =
      guarantee1.transitive(guarantee2)

}
