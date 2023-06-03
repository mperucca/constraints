import constraints.{Inlinable, Computable}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given computable[A, B](
    using a: Computable.Typed[A, Int], b: Computable.Typed[B, Int]
  ): Computable.Predicate[LessThan[A, B]] =
    Computable(a.compute < b.compute)

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

