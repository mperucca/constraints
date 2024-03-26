package constraints

abstract class Computes[C[_]](floatOperation: Float => Boolean, doubleOperation: Double => Boolean) {
  given computeFloat[A: Compute.To[Float]]: Compute.Typed[C[A], Boolean] = Compute(floatOperation(Compute[A]))
  given computeDouble[A: Compute.To[Double]]: Compute.Typed[C[A], Boolean] = Compute(doubleOperation(Compute[A]))
}

trait NeitherImplies[NotThis[_], NorThis[_], This[_]]:
  given [A](using
            Guarantee[Not[NotThis[A]]],
            Guarantee[Not[NorThis[A]]]
  ): Guarantee[This[A]] = Guarantee.trust

type IsNaN[A]
object IsNaN
  extends Computes[IsNaN](_.isNaN, _.isNaN)
    with NeitherImplies[IsFinite, IsInfinite, IsNaN]

type IsFinite[A]
object IsFinite
  extends Computes[IsFinite](_.isFinite, _.isFinite)
    with NeitherImplies[IsNaN, IsInfinite, IsFinite]

type IsInfinite[A]
object IsInfinite
  extends Computes[IsInfinite](_.isInfinite, _.isInfinite)
    with NeitherImplies[IsNaN, IsFinite, IsInfinite]

type IsWhole[A]
object IsWhole extends Computes[IsWhole](_.isWhole, _.isWhole)

type IsValidByte[A]
object IsValidByte extends Computes[IsValidByte](_.isValidByte, _.isValidByte)

type IsValidChar[A]
object IsValidChar extends Computes[IsValidChar](_.isValidChar, _.isValidChar)

type IsValidInt[A]
object IsValidInt extends Computes[IsValidInt](_.isValidInt, _.isValidInt)

type IsValidShort[A]
object IsValidShort extends Computes[IsValidShort](_.isValidShort, _.isValidShort)
