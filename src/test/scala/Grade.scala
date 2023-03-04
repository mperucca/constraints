import constraints.and

type Grade[Letter] = Between['A', Letter, 'F']

object Grade:

  type Passing[Letter] = Grade[Letter] and (Letter AtMost 'C')