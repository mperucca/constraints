import constraints.*
import constraints.compile.Inlinable

import scala.quoted.{Expr, Quotes, Type}

type Length[I]

object Length extends Compute.Companion[Length, String, Int](_.length):

  transparent inline given inlinable[S: Inlinable.To[String]]: Inlinable.Typed[Length[S], Int] =
    inline Inlinable.reduce[S] match
      case None => Inlinable.Unknown
      case Some(s: String) => Impl[s.type]

  class Impl[S <: String] extends Inlinable.Impl[Int]:
    override transparent inline def reduce: Option[Int] = ${ impl[S] }

  private def impl[S <: String : Type](using Quotes): Expr[Option[Int]] =
    Inlinable.fromComputation((s: S) => compute[s.type])