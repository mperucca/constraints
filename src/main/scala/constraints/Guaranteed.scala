package constraints

/**
 * Represents a value with an attached [[Guarantee]].
 * Useful for simple return values from functions without need to create a custom type.
 * @tparam V The value being guaranteed
 */
trait Guaranteed[+V]:

  /**
   * The value about which there is a guarantee
   */
  val value: V

  /**
   * The guarantee associated with the value
   * @return the guarantee
   */
  def guarantee: Guarantee.Impl[Any]

/**
 * Utility methods for constructing [[Guaranteed]] values
 */
object Guaranteed:

  /**
   * Helper "constructor" for [[Guaranteed]] values that accepts the value as a parameter
   * @tparam V The type of the value being guaranteed
   */
  transparent trait Value[+V](override val value: V) extends Guaranteed[V]

  /**
   * Helper "constructor" for [[Guaranteed]] values that accepts the value as a type with a [[ValueOf]] instance
   * @tparam V The value being guaranteed
   */
  transparent trait Type[+V: ValueOf] extends Guaranteed[V]:
    override val value: V = valueOf

  /**
   * Raw constructor that accepts any value and guarantee.
   * @note Like the trait definition itself, nothing ensures that the value and guarantee are necessarily related.
   * @param value the value with a guarantee
   * @param guarantee the guarantee about the value
   * @return the [[Guaranteed]] value
   */
  def apply(value: Any, guarantee: Guarantee.Impl[Any]): Guaranteed.Typed[value.type, guarantee.type] =
    val g: guarantee.type = valueOf
    new Guaranteed.Type[value.type]:
      override def guarantee: g.type = valueOf

  /**
   * Type alias that's often clearner than the structural type
   * @tparam V The type of the value being guaranteed
   * @tparam G The type of guarantee
   */
  type Typed[+V, +G] = Guaranteed[V] { def guarantee: G }

  /**
   * Type alias with the nested constraint of the guarantee exposed
   * @note Unable to have variance on [[C]] due to match type normalization
   * @tparam V the type of the value being constrained
   * @tparam C the constraint on the value
   */
  type Constrained[+V, C] = Typed[V, Guarantee[C]]

  /**
   * Type alias similar to tagging refinement systems
   *
   * @tparam V the type of the value being constrained
   * @tparam C the constraint on the value
   */
  type Refined[+V, C[_]] = Guaranteed[V] { def guarantee: Guarantee[C[value.type]] }

  object Refined:

    /**
     * Constructs a [[Refined]] value
     * @param value the value with the guarantee
     * @param guarantee the guarantee on the value
     * @tparam C the constraint
     * @return the refined value
     */
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
      Guarantee.test[C[v.type]] match
        case Left(invertedGuarantee: Guarantee[Not[C[v.type]]]) =>
          Left(Guaranteed.Refined(v)(invertedGuarantee))
        case Right(guarantee: Guarantee[C[v.type]]) =>
          Right(Guaranteed.Refined(v)(guarantee))
