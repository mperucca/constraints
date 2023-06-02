import constraints.*
import constraints.Computation.Typed

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length:

  given computation[S](
    using c: Computation.Typed[S, String]
  ): Computation.Typed[Length[S], Int] =
    Computation(c.compute.length)

  transparent inline given inliner[S](
    using c: Inliner.Typed[S, String]
  ): Inliner.Typed[Length[S], Int] =
    inline c.reduce match
      case null => Inliner.Unknown
      case s: String => InlinerImpl[s.type]

  class InlinerImpl[S <: String] extends Inliner.Impl[Int]:
    override transparent inline def reduce: Int | Null = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Int | Null] =
    Inliner.fromComputation((s: S) => computation[s.type])