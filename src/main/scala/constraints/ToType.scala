package constraints

import scala.quoted.Quotes

trait ToType[-V]:

  def apply(value: V)(using Quotes): quoted.quotes.reflect.TypeRepr

object ToType:

  def apply[V](value: V)(using toType: ToType[V])(using Quotes): quoted.quotes.reflect.TypeRepr =
    toType(value)

  given [B: Builtin]: ToType[B] with
    override def apply(builtin: B)(using Quotes): quoted.quotes.reflect.TypeRepr =
      builtin match
        case primitive: Primitive => Primitive.toConstantType(primitive)
        case tuple: Tuple => tupleToLiteralTupleType(tuple)
