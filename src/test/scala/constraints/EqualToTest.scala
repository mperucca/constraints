package constraints

import org.scalatest.funsuite.AnyFunSuite

class EqualToTest extends AnyFunSuite {

  test("NaN"):
    ComputeTest[EqualTo[Double.NaN.type, Double.NaN.type]](false)
    ComputeTest[Not[EqualTo[Double.NaN.type, Double.NaN.type]]](true)
    ComputeTest[Not[Not[EqualTo[Double.NaN.type, Double.NaN.type]]]](false)

}
