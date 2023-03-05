import constraints.{Guarantee, and, or}

type Grade[Letter] = Between['A', Letter, 'F']

object Grade:

  type Passing[Letter] = Between['A', Letter, 'C']
  type Failing[Letter] = Between['E', Letter, 'F']

  extension [L: ValueOf] (g: Guarantee[Passing[L] or Failing[L]])
    def toGrade: Guarantee[Grade[L]] = Guarantee.trust
