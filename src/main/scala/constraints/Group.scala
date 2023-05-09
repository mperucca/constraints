package constraints

import scala.quoted.{Expr, Quotes, ToExpr, Type}

/**
 * A group of extractable types/values.
 * This is equivalent to a [[Tuple]] except that it is guaranteed to be made up of only extractable types.
 *  - As a type it represents a group of types on which value extraction can be attempted.
 *  - As a value it represents a group of runtime values (possibly having been extracted from a type).
 */
sealed trait Group:

  /**
   * Turns this group into its corresponding [[Tuple]] value
   *
   * @return the tuple
   */
  def toTuple: Group.ToTuple[this.type]

/**
 * Contains the [[Group]] types
 */
object Group:

  /**
   * Represents the end of a group, analogous to [[EmptyTuple]]
   */
  object End extends Group:

    /**
     * Turns this into the [[EmptyTuple]]
     *
     *  @return [[EmptyTuple]]
     */
    def toTuple: ToTuple[this.type] = EmptyTuple

    given toExpr: ToExpr[Group.End.type] with
      def apply(group: Group.End.type)(using Quotes): Expr[Group.End.type] =
        '{ _root_.constraints.Group.End }

  /**
   * A non-empty [[Group]], analogous to [[*:]]
   *
   * @param h the head of the group
   * @param t the tail of the group
   * @tparam H the type of the head
   * @tparam T the type of the tail
   */
  case class Link[H <: Extractable, T <: Group](h: H, t: T) extends Group:

    /**
     * Turns this group into its corresponding [[NonEmptyTuple]]
     *
     *  @return the tuple
     */
    def toTuple: ToTuple[this.type] = this match
      case Link(Link(f, s), t) => (Link(f, s).toTuple *: t.toTuple).asInstanceOf[ToTuple[this.type]]
      case Link(f, s) => (f *: s.toTuple).asInstanceOf[ToTuple[this.type]]

  /**
   * Contains [[Link]] helpers
   */
  object Link:

    /**
     * The [[ValueOf]] instance for non-empty [[Group]]s
     *
     * @tparam H the type of the head
     * @tparam T the type of the tail
     * @return a [[ValueOf]] instance to generate a runtime value of a non-empty [[Group]] type
     */
    given linkValueOf[H <: Extractable : ValueOf, T <: Group : ValueOf]: ValueOf[Link[H, T]] = ValueOf(Link(valueOf[H], valueOf[T]))

    given toExpr[H <: Extractable: Type: ToExpr, T <: Group: Type: ToExpr]: ToExpr[Link[H, T]] with
      def apply(group: Link[H, T])(using Quotes): Expr[Link[H, T]] =
        group match
          case Group.Link(h, t) =>
            val head = Expr[H](h)
            val tail = Expr[T](t)
            '{ _root_.constraints.Group.Link($head, $tail) }

  /**
   * Converts a [[Tuple]] type to its corresponding [[Group]] type
   * @note Gives a compile error if the tuple [[T]] contains non-[[Extractable]] types
   *
   * @tparam T the [[Tuple]] type to convert
   */
  type FromTuple[T <: Tuple] <: Group = T match
    case EmptyTuple => End.type
    case (hh *: ht) *: t => Link[FromTuple[hh *: ht], FromTuple[t]]
    case h *: t => Link[h, FromTuple[t]]

  /**
   * Converts a [[Group]] type to its corresponding [[Tuple]] type
   *
   * @tparam A the [[Group]] type to convert
   */
  type ToTuple[A <: Group] <: Tuple = A match
    case End.type => EmptyTuple
    case Link[Link[hh, ht], t] => ToTuple[Link[hh, ht]] *: ToTuple[t]
    case Link[h, t] => h *: ToTuple[t]

  /**
   * The [[Iterate]] instance for iterating through the values of [[Group]]s
   * @note Iterates through the shallowest layer, not recursively (deeply) through the [[Primitive]] leafs
   * 
   * @return the [[Iterate]] instance for [[Group]]s
   */
  given Iterate[Group, Extractable] =
    Iterable.unfold(_) { PartialFunction.condOpt(_) { case Link(h, t) => (h, t) } }
