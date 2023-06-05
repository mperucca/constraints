import constraints.{Inlinable, Compute}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given computable[A, B](using Compute.Typed[A, Int], Compute.Typed[B, Int]): Compute.Predicate[LessThan[A, B]] =
    Compute(Compute[A] < Compute[B])

  transparent inline given inlinable[A, B](
    using a: Inlinable.Typed[A, Int], b: Inlinable.Typed[B, Int]
  ): Inlinable.Predicate[LessThan[A, B]] =
    inline a.reduce match
      case None => Inlinable.Unknown
      case Some(l: Int) =>
        inline b.reduce match
          case None => Inlinable.Unknown
          case Some(h: Int) => Impl[l.type, h.type]

  class Impl[A <: Int, B <: Int] extends Inlinable.Impl[Boolean]:
    override transparent inline def reduce: Option[Boolean] = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Option[Boolean]] =
    Inlinable.fromComputable[(A, B), Boolean]((a, b) => computable[a.type, b.type])

