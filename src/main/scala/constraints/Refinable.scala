package constraints

import scala.quoted.Quotes

trait Refinable[-V]:

  def refine(value: V)(using Quotes): quoted.quotes.reflect.TypeRepr

object Refinable:

  def apply[V](value: V)(using refinable: Refinable[V])(using Quotes): quoted.quotes.reflect.TypeRepr =
    refinable.refine(value)

  given [B: Builtin]: Refinable[B] with
    override def refine(builtin: B)(using Quotes): quoted.quotes.reflect.TypeRepr =
      builtin match
        case primitive: Primitive => Primitive.toConstantType(primitive)
        case tuple: Tuple => tupleToLiteralTupleType(tuple)
