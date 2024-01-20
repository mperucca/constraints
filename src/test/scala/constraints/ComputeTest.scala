package constraints

import org.scalatest.Assertion
import org.scalatest.Assertions.*
import org.scalatest.funsuite.AnyFunSuite

object ComputeTest:

  def apply[A](using compute: Compute[A])(expectedResult: Any)(using CanEqual[compute.Result, expectedResult.type]): Assertion =
    val result: compute.Result = compute.compute
    assert(result == expectedResult)

class ComputeTest extends AnyFunSuite:

  test("Null"):
    ComputeTest[Null](null)

  test("Some"):
    ComputeTest[Some[ComputeTest.type]](Some(ComputeTest))

  test("NonEmptyTuple"):
    ComputeTest[(1, "", false)]((1, "", false))
