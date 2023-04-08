package constraints

import scala.collection.IterableOps

/**
 * A value with an attached constraint
 * @param value the value constrained by [[C]]
 * @tparam V the type being constrained
 * @tparam C the constraint
 */
infix class Constrained[+V, C[_]] private(val value: V) extends AnyVal:

  /**
   * Gets the guarantee (which must exist since [[Constrained.apply]] needed it to construct this instance)
   * @return the guarantee that the constraint holds
   */
  def guarantee: Guarantee[C[value.type]] = Guarantee.trust

/**
 * Utility methods for constructing [[Constrained]] values
 */
object Constrained:

  /**
   * Constructs a [[Constrained]] value with nicer inference than the constructor
   * @param v the value to constrain
   * @param guarantee evidence of the constraint
   * @tparam C the constraint
   * @return the constrained value
   */
  def apply[C[_]](v: Any)(guarantee: Guarantee[C[v.type]]) =
    new Constrained[v.type, C](v)

  /**
   * Partitions an iterable into a [[Tuple2]] where
   * the 1st item contains values and inverse [[Guarantee]]s that the inverse of constraint [[C]] holds
   * the 2nd item contains values and [[Guarantee]]s that the constraint [[C]] holds
   *
   * @param iterable the values to check constraint [[P]] on
   * @param runtimeCheck the runtime check constraint to perform
   * @param I[V] <:< IterableOps[V, I, I[V]] evidence that the values can be iterated over
   * @tparam I the iterable type
   * @tparam V the value type inside the iterable
   * @tparam C the constraint to check on each value
   * @return a tuple containing
   */
  def partition[I[_], V, C[_]](iterable: I[V])(runtimeCheck: (a: V) => RuntimeCheck[C[a.type]])(
    using I[V] <:< IterableOps[V, I, I[V]]
  ): (I[V Constrained Inverse[C]], I[V Constrained C]) =
    iterable.partitionMap { v =>
      Guarantee.runtimeCheck(using runtimeCheck(v)) match
        case Left(invertedGuarantee: Guarantee[Not[C[v.type]]]) =>
          Left(Constrained(v)(invertedGuarantee))
        case Right(guarantee: Guarantee[C[v.type]]) =>
          Right(Constrained(v)(guarantee))
    }
