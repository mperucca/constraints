package constraints

import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps

@targetName("LessThan")
type <[A, B]

object LessThan extends Inequality[<]([A] => (_: Ordering[A]) ?=> _ < _)

@targetName("GreaterThan")
type >[A, B]

object GreaterThan extends Inequality[>]([A] => (_: Ordering[A]) ?=> _ > _)

@targetName("LessThanOrEqualTo")
type <=[A, B]

object LessThanOrEqualTo extends Inequality[<=]([A] => (_: Ordering[A]) ?=> _ <= _)

@targetName("GreaterThanOrEqualTo")
type >=[A, B]

object GreaterThanOrEqualTo extends Inequality[>=]([A] => (_: Ordering[A]) ?=> _ >= _)

trait Inequality[op[_, _]](op: [A] => Ordering[A] ?=> (A, A) => Boolean) {

  given compute[A: Compute.To[C], B: Compute.To[C], C: Ordering]: Compute.Predicate[A op B] =
    Compute(op(Compute[A], Compute[B]))

}
