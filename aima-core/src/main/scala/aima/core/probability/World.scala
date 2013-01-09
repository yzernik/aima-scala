package aima.core.probability

/**
 * Represents a set of assignments, i.e. a possible world.
 *
 * @author Alex DiCarlo
 */
trait World {
  type RV[A] <: RandomVariable[A]
  type Assign[A] <: Assignment[A]
  val assignments: Map[RV[_], Assign[_]]
  require(assignments forall {case (variable, assign) => variable.name == assign.variable.name},
    "Assignments do not have correct map! Some key != value.variable.num")
  def apply[A](variable: RV[A]): Assign[A] =
    assignments(variable.asInstanceOf[RV[_]]).asInstanceOf[Assign[A]]
  def get[A](variable: RV[A]): Option[Assign[A]] =
    assignments.get(variable.asInstanceOf[RV[_]]).asInstanceOf[Option[Assign[A]]]
}

object World {
  type AnyWorld = World {type RV[A] = RandomVariable[A]; type Assign[A] = Assignment[A]}

  implicit def world2AnyWorld(world: World): AnyWorld = new World {
    type RV[A] = RandomVariable[A]
    type Assign[A] = Assignment[A]
    val assignments: Map[RV[_], Assign[_]] = world.assignments.toMap
  }
}