package constraints

trait Iterate[-I, +A]:
  def iterable(i: I): Iterable[A]
  extension (i: I)
    def toIterable: Iterable[A] = iterable(i)

object Iterate:

  given [T <: Tuple, A](using Tuple.Union[T] <:< A): Iterate[T, A] with
    override def iterable(i: T): Iterable[A] = new Iterable[A]:
      override def iterator: Iterator[A] =
        new Iterator[A]:
          var cur: Tuple = i

          override def hasNext: Boolean = cur match
            case EmptyTuple => false
            case _: NonEmptyTuple => true

          override def next(): A = cur match
            case EmptyTuple => throw java.util.NoSuchElementException()
            case head *: tail =>
              cur = tail
              head.asInstanceOf[A]

  given [A]: Iterate[Iterable[A], A] with
    override def iterable(i: Iterable[A]): Iterable[A] = i

  given Iterate[String, Int] with
    override def iterable(i: String): Iterable[Int] = new Iterable[Int]:
      override def iterator: Iterator[Int] = new Iterator[Int]:
        val it: java.util.PrimitiveIterator.OfInt = i.codePoints().iterator()
        override def hasNext: Boolean = it.hasNext
        override def next(): Int = it.nextInt()

