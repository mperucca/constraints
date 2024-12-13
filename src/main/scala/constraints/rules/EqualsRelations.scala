package constraints.rules

import constraints.{==, Compute, Guarantee, IsNaN, Not}

trait Reflexive[-A]

object Reflexive {

  given Reflexive[Boolean] = new Reflexive[Boolean] {}

  given Reflexive[Byte] = new Reflexive[Byte] {}

  given Reflexive[Char] = new Reflexive[Char] {}

  given Reflexive[Int] = new Reflexive[Int] {}

  given Reflexive[Long] = new Reflexive[Long] {}

  given Reflexive[Short] = new Reflexive[Short] {}

  given Reflexive[String] = new Reflexive[String] {}

  given [F <: Float | Double](using Guarantee[Not[IsNaN[F]]]): Reflexive[F] = new Reflexive[F] {}
  
  def apply[A: Compute : Reflexive]: Guarantee[A == A] = Guarantee.trust

}

trait Symmetric[-A, -B]

object Symmetric {

  given Symmetric[Boolean, Boolean] = new Symmetric[Boolean, Boolean] {}

  given Symmetric[Byte, Byte] = new Symmetric[Byte, Byte] {}

  given Symmetric[Char, Char] = new Symmetric[Char, Char] {}

  given Symmetric[Double, Double] = new Symmetric[Double, Double] {}

  given Symmetric[Float, Float] = new Symmetric[Float, Float] {}

  given Symmetric[Int, Int] = new Symmetric[Int, Int] {}

  given Symmetric[Long, Long] = new Symmetric[Long, Long] {}

  given Symmetric[Short, Short] = new Symmetric[Short, Short] {}

  given Symmetric[String, String] = new Symmetric[String, String] {}

  def apply[A, B](guarantee: Guarantee[A == B])(using Symmetric[A, B]): Guarantee[B == A] =
    Guarantee.trust

}

trait Transitive[-A, -B, -C]

object Transitive {

  given Transitive[Boolean, Boolean, Boolean] = new Transitive[Boolean, Boolean, Boolean] {}

  given Transitive[Byte, Byte, Byte] = new Transitive[Byte, Byte, Byte] {}

  given Transitive[Char, Char, Char] = new Transitive[Char, Char, Char] {}

  given Transitive[Double, Double, Double] = new Transitive[Double, Double, Double] {}

  given Transitive[Float, Float, Float] = new Transitive[Float, Float, Float] {}

  given Transitive[Int, Int, Int] = new Transitive[Int, Int, Int] {}

  given Transitive[Long, Long, Long] = new Transitive[Long, Long, Long] {}

  given Transitive[Short, Short, Short] = new Transitive[Short, Short, Short] {}

  given Transitive[String, String, String] = new Transitive[String, String, String] {}

  def apply[A, B, C](
    guarantee: Guarantee[A == B],
    other: Guarantee[B == C]
  )(using Transitive[A, B, C]): Guarantee[A == C] =
    Guarantee.trust

}
