package constraints

import scala.collection.IterableOps

/**
 * A value with an attached constraint
 * @param value the value constrained by [[P]]
 * @param trust evidence of the constraint on the [[value]]
 * @tparam A the type being constrained
 * @tparam P the constraint
 */
class Constrained[+A, P[_ <: A]](val value: A)(val trust: Trust[P[value.type]])

/**
 * Utility methods for constructing [[Constrained]] values
 */
object Constrained:

  /**
   * Constructs a [[Constrained]] value with nicer inference than the constructor
   * @note [[P]] cannot have type bounds with this construction method
   * @param a the value to constrain
   * @param trust evidence of the constraint
   * @tparam P the constraint
   * @return the constrained value
   */
  def apply[P[_]](a: Any)(trust: Trust[P[a.type]]): Constrained[a.type, P] = new Constrained(a)(trust)

  /**
   * Constructs a function that creates [[Constrained]] values with nicer inference than the constructor
   * @param a the value to constrain
   * @return a function that creates a constrained value when supplied constraint evidence
   */
  def apply(a: Any): [P[_ <: a.type]] => Trust[P[a.type]] => Constrained[a.type, P] =
    [P[_ <: a.type]] => (trust: Trust[P[a.type]]) => new Constrained[a.type, P](a)(trust)

  /**
   * Partitions an iterable into a [[Tuple2]] where
   * the 1st item contains values and inverse [[Trust]] evidences that the inverse of constraint [[P]] holds
   * the 2nd item contains values and [[Trust]] evidences that the constraint [[P]] holds
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
      Trust.runtimeCheck(using runtimeCheck(a)) match
        case Left(distrust: Trust[not[P[a.type]]]) => Left(new Constrained(a)(distrust))
        case Right(trust: Trust[P[a.type]]) => Right(new Constrained(a)(trust))
    }
