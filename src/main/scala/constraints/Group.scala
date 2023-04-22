package constraints

import scala.quoted.{Expr, Quotes, Type}

sealed trait Group:

  def toTuple: Group.ToTuple[this.type]

object Group:

  object End extends Group:
    def toTuple: ToTuple[this.type] = EmptyTuple

  case class Link[H <: Extractable, T <: Group](h: H, t: T) extends Group:
    def toTuple: ToTuple[this.type] = this match
      case Link(Link(f, s), t) => (Link(f, s).toTuple *: t.toTuple).asInstanceOf[ToTuple[this.type]]
      case Link(f, s) => (f *: s.toTuple).asInstanceOf[ToTuple[this.type]]

  object Link:
    given linkValueOf[H <: Extractable : ValueOf, T <: Group : ValueOf]: ValueOf[Link[H, T]] = ValueOf(Link(valueOf[H], valueOf[T]))

  type FromTuple[T <: Tuple] <: Group = T match
    case EmptyTuple => End.type
    case (hh *: ht) *: t => Link[FromTuple[hh *: ht], FromTuple[t]]
    case h *: t => Link[h, FromTuple[t]]

  type ToTuple[A <: Group] <: Tuple = A match
    case End.type => EmptyTuple
    case Link[Link[hh, ht], t] => ToTuple[Link[hh, ht]] *: ToTuple[t]
    case Link[h, t] => h *: ToTuple[t]

  given Iterate[Group, Extractable] =
    Iterable.unfold(_) { PartialFunction.condOpt(_) { case Link(h, t) => (h, t) } }
