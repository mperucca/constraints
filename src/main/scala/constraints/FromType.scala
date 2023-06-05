package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
trait FromType[E]:

  def extract(using Quotes): Option[E]

/**
 * Holds the extraction method
 */
object FromType:

  given builtinSingleton[B <: Singleton: Builtin : Type]: FromType[B] with
    override def extract(using Quotes): Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  given builtin[B: Builtin : Type]: FromType[B] with
    override def extract(using Quotes): Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  given nonEmptyTuple[H, T <: Tuple](using h: FromType[H], t: FromType[T]): FromType[H *: T] with
    override def extract(using Quotes): Option[H *: T] =
      for head <- h.extract
          tail <- t.extract
        yield head *: tail
