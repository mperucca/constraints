package constraints

import scala.quoted.*

/**
 * Type class from creating a type from a value at compile time
 * @tparam V the type of values that can be converted to, presumably more narrow/literal/constant, types
 */
trait ToType[V]:

  /**
   * Creates a type from a value. The resulting type is intended to be narrowed to its constant value.
   * @param value the value to create a type from
   * @param Quotes for performing macro operations
   * @return the most specific type possible for the value
   */
  def apply(value: V)(using Quotes): Type[? <: V]

object ToType:

  /**
   * Convenience method for creating a type from a value
   * @param value the value to create a type from
   * @param toType the type class instance for lifting the value to the type
   * @param Quotes for performing macro operations
   * @tparam V the widened type of the value
   * @return the narrowed type of the value
   */
  def apply[V](value: V)(using toType: ToType[V])(using Quotes): Type[? <: V] =
    toType(value)

  /**
   * instance for converting wide [[Builtin]] values to narrow types
   */
  given [B: Builtin]: ToType[B] with
    override def apply(builtin: B)(using Quotes): Type[? <: B] =
      import quoted.quotes.reflect.*
      val tpe = builtin match
        case primitive: Primitive =>
          val constant = primitive match
            case b: Boolean => BooleanConstant(b)
            case b: Byte => ByteConstant(b)
            case s: Short => ShortConstant(s)
            case i: Int => IntConstant(i)
            case l: Long => LongConstant(l)
            case f: Float => FloatConstant(f)
            case d: Double => DoubleConstant(d)
            case c: Char => CharConstant(c)
            case s: String => StringConstant(s)
          ConstantType(constant).asType
        case tuple: Tuple =>
          tuple match
            case EmptyTuple => Type.of[EmptyTuple]
            case h *: t =>
              given Builtin[h.type] = Builtin[h.type]
              given Builtin[t.type] = Builtin[t.type]
              val head = ToType[h.type](h)
              val tail = ToType[t.type](t)
              AppliedType(TypeRepr.of[*:[_, _]], List(TypeRepr.of(using head), TypeRepr.of(using tail))).asType
      tpe.asInstanceOf[Type[? <: B]]
