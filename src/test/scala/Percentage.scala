import constraints.{Guaranteed, Guarantee, and}

type Percentage[D <: Double] = Guaranteed[D, Percentage.Constraint]

object Percentage:

  type Constraint[Value] = Between[0d, Value, 1d]

  def apply(d: Double)(guarantee: Guarantee[Percentage.Constraint[d.type]]): Percentage[d.type] =
    Guaranteed(d)(guarantee)

  inline def compileTimeCheck(d: Double): Percentage[d.type] =
    Guaranteed(d)(Guarantee.verifyAtCompileTime)
