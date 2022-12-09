package constraints

type Normalize[A] = A match
  case Not[Not[a]] => Normalize[a]
  case Not[a And b] => Normalize[Not[a] Or Not[b]]
  case Not[a Or b] => Normalize[Not[a] And Not[b]]
  case Not[a Xor b] => Normalize[Not[TranslateXor[a, b]]]
  case a And b => Normalize[a] & Normalize[b]
  case a Or b => Normalize[a] | Normalize[b]
  case a Xor b => Normalize[TranslateXor[a, b]]
  case Not[False] => True
  case Not[True] => False
  case _ => A
private type TranslateXor[A, B] = (A And Not[B]) Or (Not[A] And B)
