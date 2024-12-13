package constraints

import org.scalatest.funsuite.AnyFunSuite

class EqualToTest extends AnyFunSuite {

  test("NaN"):
    ComputeTest[Double.NaN.type == Double.NaN.type](false)
    ComputeTest[Double.NaN.type != Double.NaN.type](true)
    ComputeTest[![Double.NaN.type != Double.NaN.type]](false)

}
