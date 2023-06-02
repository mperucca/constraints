import constraints.{Inliner, Computation}

import scala.quoted.*

type LessThan[A, B]

object LessThan:

  given computation[A, B](
    using a: Computation.Typed[A, Int], b: Computation.Typed[B, Int]
  ): Computation.Predicate[LessThan[A, B]] =
    Computation(a.compute < b.compute)

  transparent inline given inliner[A, B](
    using a: Inliner.Typed[A, Int], b: Inliner.Typed[B, Int]
  ): Inliner.Predicate[LessThan[A, B]] =
    inline a.reduce match
      case null => Inliner.Unknown
      case l: Int =>
        inline b.reduce match
          case null => Inliner.Unknown
          case h: Int => Impl[l.type, h.type]

  class Impl[A <: Int, B <: Int] extends Inliner.Impl[Boolean]:
    override transparent inline def reduce: Boolean | Null = ${ impl[A, B] }

  private def impl[A <: Int : Type, B <: Int: Type](using Quotes): Expr[Boolean | Null] =
    Inliner.fromComputation[(A, B), Boolean] { case (a, b) =>
      computation[a.type, b.type]
    }

