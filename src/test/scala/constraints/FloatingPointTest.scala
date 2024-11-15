package constraints

import org.scalatest.funsuite.AnyFunSuite

class FloatingPointTest extends AnyFunSuite:

  test("IsNaN"):
    ComputeTest[IsNaN[1d]](false)
    ComputeTest[IsNaN[Double.NaN.type]](true)
    ComputeTest[IsNaN[Double.PositiveInfinity.type]](false)
    type V
    given notFinite: Guarantee[Not[IsFinite[V]]] = Guarantee.trust
    given notInfinite: Guarantee[Not[IsInfinite[V]]] = Guarantee.trust
//    summon[Guarantee[IsNaN[V]]]

  test("IsFinite"):
    ComputeTest[IsFinite[1d]](true)
    ComputeTest[IsFinite[Double.NaN.type]](false)
    ComputeTest[IsFinite[Double.PositiveInfinity.type]](false)
    type V
    given notNaN: Guarantee[Not[IsNaN[V]]] = Guarantee.trust
    given notInfinite: Guarantee[Not[IsInfinite[V]]] = Guarantee.trust
//    summon[Guarantee[IsFinite[V]]]

  test("IsInfinite"):
    ComputeTest[IsInfinite[1d]](false)
    ComputeTest[IsInfinite[Double.NaN.type]](false)
    ComputeTest[IsInfinite[Double.PositiveInfinity.type]](true)
    type V
    given notNaN: Guarantee[Not[IsNaN[V]]] = Guarantee.trust
    given notFinite: Guarantee[Not[IsFinite[V]]] = Guarantee.trust
//    summon[Guarantee[IsInfinite[V]]]
