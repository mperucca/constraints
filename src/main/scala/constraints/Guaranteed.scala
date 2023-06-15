package constraints

trait Guaranteed[+V](val value: V):

  def guarantee: Guarantee.Impl[Any]

/**
 * Utility methods for constructing [[Guaranteed]] values
 */
object Guaranteed:

  def apply(value: Any, guarantee: Guarantee.Impl[Any]): Guaranteed.Typed[value.type, guarantee.type] =
    val g: guarantee.type = valueOf
    new Guaranteed[value.type](valueOf):
      override def guarantee: g.type = valueOf

  type Typed[+V, +G] = Guaranteed[V] { def guarantee: G }

  type Constrained[+V, C] = Typed[V, Guarantee[C]]

  type Refined[+V, C[_]] = Guaranteed[V] { def guarantee: Guarantee[C[value.type]] }

  object Refined:
    def apply[C[_]](value: Any)(guarantee: Guarantee[C[value.type]]): Refined[value.type, C] =
      Guaranteed(value, guarantee)

    /**
     * Checks constraint [[C]] on value [[v]] at runtime
     *
     * @param v the value to check
     * @param c the runtime check to run
     * @tparam C the constraint to check
     * @return a constrained value:
     *         if the constraint check fails
     *          - a [[Left]] containing the value and an inverse [[Guarantee]] that the inverse of constraint [[C]] holds
     *            otherwise
     *          - a [[Right]] containing the value and a [[Guarantee]] that constraint [[C]] holds
     */
    def runtimeCheck[C[_]](v: Any)(
      using c: Compute.Typed[C[v.type], Boolean]
    ): Either[Guaranteed.Refined[v.type, Inverse[C]], Guaranteed.Refined[v.type, C]] =
      Guarantee.testAtRuntime[C[v.type]] match
        case Left(invertedGuarantee: Guarantee[Not[C[v.type]]]) =>
          Left(Guaranteed.Refined(v)(invertedGuarantee))
        case Right(guarantee: Guarantee[C[v.type]]) =>
          Right(Guaranteed.Refined(v)(guarantee))
