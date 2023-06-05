import constraints.{Inlinable, Compute}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given computable[A: Compute.To[Int], B: Compute.To[Int]]: Compute.Typed[LessThan[A, B], Boolean] =
    Compute(Compute[A] < Compute[B])

  transparent inline given inlinable[A: Inlinable.To[Int], B: Inlinable.To[Int]]: Inlinable.Typed[LessThan[A, B], Boolean] =
    inline Inlinable[A] match
      case None => Inlinable.Unknown
      case Some(l: Int) =>
        inline Inlinable[B] match
          case None => Inlinable.Unknown
          case Some(h: Int) => Impl[l.type, h.type]

  class Impl[A <: Int, B <: Int] extends Inlinable.Impl[Boolean]:
    override transparent inline def reduce: Option[Boolean] = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Option[Boolean]] =
    Inlinable.fromComputable[(A, B), Boolean]((a, b) => computable[a.type, b.type])

