package constraints

import org.scalatest.Assertion
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite

object ComputeTest:

  def apply[A](using compute: Compute[A])(expectedResult: Any)(using equalTo: EqualTo[compute.Result, expectedResult.type]): Assertion =
    val result: compute.Result = compute.compute
    assert(equalTo(result, expectedResult))

class ComputeTest extends AnyFunSuite:

  test("Some"):
    ComputeTest[Some[0]](Some(0))

  test("NonEmptyTuple"):
    ComputeTest[(1, "", false)]((1, "", false))
