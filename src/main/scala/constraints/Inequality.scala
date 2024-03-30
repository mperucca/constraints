package constraints

import scala.annotation.targetName

infix type LessThan[A, B]
@targetName("GreaterThan")
type <[A, B] = LessThan[A, B]

object LessThan {

  given computeInt[A: Compute.To[Int], B: Compute.To[Int]]: Compute.Typed[A < B, Boolean] =
    Compute(Compute[A] < Compute[B])

  given computeDouble[A <: Double: ValueOf, B <: Double: ValueOf](
    using Guarantee[Not[IsNaN[A]]], Guarantee[Not[IsNaN[B]]]
  ): Compute.Typed[A < B, Boolean] =
    Compute(valueOf[A] < valueOf[B])

}

type GreaterThan[A, B]
@targetName("GreaterThan")
type >[A, B] = GreaterThan[A, B]

object GreaterThan {

  given compute[A: Compute.To[Int], B: Compute.To[Int]]: Compute.Typed[A < B, Boolean] =
    Compute(Compute[A] > Compute[B])

  given computeDouble[A <: Double : ValueOf, B <: Double : ValueOf](
    using Guarantee[Not[IsNaN[A]]], Guarantee[Not[IsNaN[B]]]
  ): Compute.Typed[A > B, Boolean] =
    Compute(valueOf[A] > valueOf[B])

}

type LessThanOrEqualTo[A, B] = Not[GreaterThan[A, B]]
@targetName("LessThanOrEqualTo")
type <=[A, B] = LessThanOrEqualTo[A, B]

type GreaterThanOrEqualTo[A, B] = Not[LessThan[A, B]]
@targetName("GreaterThanOrEqualTo")
type >=[A, B] = GreaterThanOrEqualTo[A, B]
