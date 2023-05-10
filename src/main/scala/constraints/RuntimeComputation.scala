package constraints

trait RuntimeComputation[-E]:
  type Result
  val result: Result

object RuntimeComputation:

  type Typed[-E, +R] = RuntimeComputation[E] { type Result <: R }

  def apply[E, R](result: R): RuntimeComputation.Typed[E, R] =
    val _result: result.type = valueOf
    new RuntimeComputation[E] { type Result = R; val result: R = _result }

  given constantSingleton[R <: Singleton: ValueOf]: RuntimeComputation.Typed[R, R] = constant

  given constant[R: ValueOf]: RuntimeComputation.Typed[R, R] = RuntimeComputation(valueOf[R])
