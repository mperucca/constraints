import constraints.*
import constraints.Compute.Typed

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length:

  given computable[S: Compute.To[String]]: Compute.Typed[Length[S], Int] =
    Compute(Compute[S].length)

  transparent inline given inlinable[S: Inlinable.To[String]]: Inlinable.Typed[Length[S], Int] =
    inline Inlinable.reduce[S] match
      case None => Inlinable.Unknown
      case Some(s: String) => Impl[s.type]

  class Impl[S <: String] extends Inlinable.Impl[Int]:
    override transparent inline def reduce: Option[Int] = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Option[Int]] =
    Inlinable.fromComputation((s: S) => computable[s.type])