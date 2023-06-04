package constraints

import scala.quoted.*

/**
 * Types for which value extraction has been implemented
 */
trait Extractable[E]:

  def extract: Option[E]

/**
 * Holds the extraction method
 */
object Extractable:

  given builtinSingleton[B <: Singleton: Builtin : Type] (using Quotes): Extractable[B] with
    override def extract: Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  given builtin[B: Builtin : Type] (using Quotes): Extractable[B] with
    override def extract: Option[B] =
      Builtin.unapply(quotes.reflect.TypeRepr.of[B]).map(_.asInstanceOf[B])

  given[N <: H *: T, H <: Singleton, T <: Tuple](using h: Extractable[H], t: Extractable[T]): Extractable[N] with
    override def extract: Option[N] =
      for head <- h.extract
          tail <- t.extract
        yield (head *: tail).asInstanceOf[N]
