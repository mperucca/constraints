import constraints.and

type Between[Minimum, Value, Maximum] = (Value AtLeast Minimum) and (Value AtMost Maximum)
