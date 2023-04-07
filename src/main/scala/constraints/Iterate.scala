package constraints

/**
 * A type class for data types that produce iterables. Abstracting over this type instead of [[Iterable]] means that
 * compile time checked constraints on iterable-like constant types can more easily be preserved.
 * @tparam I the type that generates an [[Iterable]]
 * @tparam V the type generated by the [[Iterable]]
 */
trait Iterate[-I, +V]:

  /**
   * Get the iterable
   * @param i the value that can be iterated over
   * @return the iterable
   */
  def iterable(i: I): Iterable[V]

  extension (i: I)

    /**
     * Alternative method that forwards to [[iterable]]
     */
    def toIterable: Iterable[V] = iterable(i)

/**
 * Contains various [[Iterate]] type class instances
 */
object Iterate:

  /**
   * The type class instance of [[Iterate]] for tuples of the lowest upper bound of their type union
   */
  given [T <: Tuple, V](using Tuple.Union[T] <:< V): Iterate[T, V] = _.toList.asInstanceOf[Iterable[V]]

  /**
   * The type class instance of [[Iterate]] for [[Iterable]]s
   * @tparam V the type of items in the [[Iterable]]
   * @return the type class instance of [[Iterate]] for [[Iterable]]s
   */
  given [V]: Iterate[Iterable[V], V] = identity(_)

  /**
   * The type class instance of [[Iterate]] for [[String]]s
   * @return the type class instance of [[Iterate]] for [[String]]s
   */
  given Iterate[String, Int] = _.codePoints().nn.toArray.nn
