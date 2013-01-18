/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

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