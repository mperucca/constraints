package constraints

import scala.quoted.*

trait Literable[V]:

  def toLiteralType(value: V)(using Quotes): quoted.quotes.reflect.TypeRepr

object Literable:

  given [V: Builtin]: Literable[V] with
    override def toLiteralType(value: V)(using Quotes): quoted.quotes.reflect.TypeRepr =
      value match
        case p: Primitive => Primitive.toConstantType(p)
        case t: Tuple => tupleToLiteralTupleType(t)
