package constraints

import scala.quoted.*

trait ToType[V]:

  def apply(value: V)(using Quotes): Type[? <: V]

object ToType:

  def apply[V](value: V)(using toType: ToType[V])(using Quotes): Type[? <: V] =
    toType(value)

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
