package constraints

object InlinableTest:

  transparent inline def apply[A](using inlinable: Inlinable[A]): Option[inlinable.Result] =
    inlinable.reduce
