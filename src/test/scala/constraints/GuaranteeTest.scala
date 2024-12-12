package constraints

import constraints.compile.*
import org.scalatest.funsuite.AnyFunSuite

class GuaranteeTest extends AnyFunSuite:

  test("trust"):
    type A
    Guarantee.trust[A]

  test("testAtRuntime true"):
    type A
    given Compute[A] with
      override type Result = Boolean
      override def compute: Boolean = true
    val guarantee: Either[Guarantee[Not[A]], Guarantee[A]] = Guarantee.test
    assert(guarantee.isRight)

  test("testAtRuntime false"):
    type A
    given Compute[A] with
      override type Result = Boolean
      override def compute: Boolean = false
    val guarantee: Either[Guarantee[Not[A]], Guarantee[A]] = Guarantee.test
    assert(guarantee.isLeft)

  test("and"):
    type A
    type B
    val guarantee1: Guarantee[A] = Guarantee.trust
    val guarantee2: Guarantee[B] = Guarantee.trust
    guarantee1 and guarantee2: Guarantee[A and B]

  test("accumulate fails first"):
    val result = Guarantee
      .accumulateWhileFailingWith[Int]
        .test[true](_ => 1)
        .test[false](_ => 2)
      .result
    assert(result == Left(::(2, Nil)))

  test("accumulate fails multiple"):
    val result = Guarantee
      .accumulateWhileFailingWith[Int]
        .test[true](_ => 1)
        .test[false or false](_ => 2)
        .test[false and false](_ => 3)
      .result
    assert(result == Left(::(2, List(3))))

  test("accumulate passes"):
    val result: Either[::[Int], Guarantee[true and (true or true) and (true and true)]] = Guarantee
      .accumulateWhileFailingWith[Int]
        .test[true](_ => 1)
        .test_[true or true](2)
        .apply[true and true](_ ?=> 3)
      .result
    assert(result.isRight)

  test("accumulate contextual apply"):
    val result: Either[::[Int], Guarantee[true and (true or true) and (true and true)]] = Guarantee
      .accumulateWhileFailingWith[Int]
        [true](_ ?=> 1)
        [true or true](_ ?=> 2)
        [true and true](_ ?=> 3)
      .result
    assert(result.isRight)
