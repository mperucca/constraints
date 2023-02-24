package constraints

import scala.collection.IterableOps

/**
 * A value with an attached constraint
 * @param value the value constrained by [[P]]
 * @tparam A the type being constrained
 * @tparam P the constraint
 */
class Constrained[+A, P[_]] private (val value: A) extends AnyVal:

  /**
   * Gets the guarantee (which must exist as a factory method must have been used to construct this instance)
   * @return evidence of the constraint
   */
  def guarantee: Guarantee[P[value.type]] = Guarantee.trust

/**
 * Utility methods for constructing [[Constrained]] values
 */
object Constrained:

  /**
   * Constructs a [[Constrained]] value with nicer inference than the constructor
   * @param a the value to constrain
   * @param guarantee evidence of the constraint
   * @tparam P the constraint
   * @return the constrained value
   */
  def apply[P[_]](a: Any)(using Guarantee[P[a.type]]) = new Constrained[a.type, P](a)

  /**
   * Partitions an iterable into a [[Tuple2]] where
   * the 1st item contains values and inverse [[Guarantee]] evidences that the inverse of constraint [[P]] holds
   * the 2nd item contains values and [[Guarantee]] evidences that the constraint [[P]] holds
   *
   * @param iterable the values to check constraint [[P]] on
   * @param runtimeCheck the runtime check constraint to perform
   * @param I[A] <:< IterableOps[A, I, I[A]] evidence that the values can be iterated over
   * @tparam I the iterable type
   * @tparam A the value type inside the iterable
   * @tparam P the constraint to check on each value
   * @return a tuple containing
   */
  def partition[I[_], A, P[_]](iterable: I[A])(runtimeCheck: (a: A) => RuntimeCheck[P[a.type]])(
    using I[A] <:< IterableOps[A, I, I[A]]
  ): (I[A Constrained Inverse[P]], I[A Constrained P]) =
    iterable.partitionMap { a =>
      Guarantee.runtimeCheck(using runtimeCheck(a)) match
        case Left(given Guarantee[not[P[a.type]]]) => Left(Constrained(a))
        case Right(given Guarantee[P[a.type]]) => Right(Constrained(a))
    }
