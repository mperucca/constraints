import constraints.{Inlinable, Computation}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given computation[A, B](
    using a: Computation.Typed[A, Int], b: Computation.Typed[B, Int]
  ): Computation.Predicate[LessThan[A, B]] =
    Computation(a.compute < b.compute)

  transparent inline given inlinable[A, B](
    using a: Inlinable.Typed[A, Int], b: Inlinable.Typed[B, Int]
  ): Inlinable.Predicate[LessThan[A, B]] =
    inline a.reduce match
      case null => Inlinable.Unknown
      case l: Int =>
        inline b.reduce match
          case null => Inlinable.Unknown
          case h: Int => Impl[l.type, h.type]

  class Impl[A <: Int, B <: Int] extends Inlinable.Impl[Boolean]:
    override transparent inline def reduce: Boolean | Null = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Boolean | Null] =
    Inlinable.fromComputation[(A, B), Boolean] { case (a, b) =>
      computation[a.type, b.type]
    }

