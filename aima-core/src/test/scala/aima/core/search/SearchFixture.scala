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

package aima.core.search

import org.scalatest.{GivenWhenThen, FeatureSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalamock.scalatest.MockFactory

private[search] trait SearchFixture extends FeatureSpec with
                            GivenWhenThen with
                            ShouldMatchers with
                            GeneratorDrivenPropertyChecks with
                            MockFactory {
  abstract class StringAction(val addition: String)
  case object AddA extends StringAction("A")
  case object AddB extends StringAction("B")
  case object AddC extends StringAction("C")

  private def actions(state: String): Seq[StringAction] =
    if (state.length < 2) Seq(AddA, AddB, AddC) else Seq()
  private def result(state: String, action: StringAction): String = state + action.addition
  private def goalTest(state: String): Boolean = state == "C"
  private def stepCost(s: String, action: StringAction, statePrime: String): Double = 1.0
  val stringProblem: Problem[String, StringAction] = Problem("", actions, result, goalTest, stepCost)
  val initialNode: Node[String, StringAction] = Node(stringProblem.initialState, None, None, 0.0)

  def childNode(parent: Node[String, StringAction], action: StringAction): Node[String, StringAction] =
    Node.createChildNode(stringProblem, parent, action)
  def createChildren(parent: Node[String, StringAction]): Seq[Node[String, StringAction]] =
    Seq(childNode(parent, AddA), childNode(parent, AddB), childNode(parent, AddC))
}
