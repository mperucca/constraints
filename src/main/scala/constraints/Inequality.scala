package constraints

import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps

infix trait LessThan[A, B]:
  def apply(a: A, b: B): Boolean
@targetName("LessThan")
type <[A, B] = LessThan[A, B]

object LessThan {
  
  def fromOrdering[A: Ordering]: A < A = _ < _

  given computeInt[A: Compute.To[Int], B: Compute.To[Int]]: Compute.Predicate[A < B] =
    Compute(Compute[A] < Compute[B])

  given computeDouble[A <: Double: ValueOf, B <: Double: ValueOf](
    using Guarantee[Not[IsNaN[A]]], Guarantee[Not[IsNaN[B]]]
  ): Compute.Predicate[A < B] =
    Compute(valueOf[A] < valueOf[B])

}

infix trait GreaterThan[A, B]:
  def apply(a: A, b: B): Boolean
@targetName("GreaterThan")
type >[A, B] = GreaterThan[A, B]

object GreaterThan {

  def fromOrdering[A: Ordering]: A > A = _ > _

  given compute[A: Compute.To[Int], B: Compute.To[Int]]: Compute.Predicate[A < B] =
    Compute(Compute[A] > Compute[B])

  given computeDouble[A <: Double : ValueOf, B <: Double : ValueOf](
    using Guarantee[Not[IsNaN[A]]], Guarantee[Not[IsNaN[B]]]
  ): Compute.Predicate[A > B] =
    Compute(valueOf[A] > valueOf[B])

}

type LessThanOrEqualTo[A, B] = Not[GreaterThan[A, B]]
@targetName("LessThanOrEqualTo")
type <=[A, B] = LessThanOrEqualTo[A, B]

type GreaterThanOrEqualTo[A, B] = Not[LessThan[A, B]]
@targetName("GreaterThanOrEqualTo")
type >=[A, B] = GreaterThanOrEqualTo[A, B]
