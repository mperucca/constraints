package constraints

import org.scalatest.Assertion
import org.scalatest.Assertions.*

object ComputeTest:

  def apply[A](using compute: Compute[A])(expectedResult: Any)(using CanEqual[compute.Result, expectedResult.type]): Assertion =
    val result: compute.Result = compute.compute
    assert(result == expectedResult)
