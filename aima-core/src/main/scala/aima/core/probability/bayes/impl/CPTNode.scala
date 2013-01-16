/*
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

package aima.core.probability.bayes.impl

import aima.core.probability.bayes.FiniteNode
import aima.core.probability.impl.FiniteRandomVariable

/**
 * Default implementation of the FiniteNode interface that uses a fully
 * specified Conditional Probability Table to represent the Node's conditional
 * distribution.
 *
 * @author Alex DiCarlo
 */
final class CPTNode[A] private (
  val variable: FiniteRandomVariable[A],
  val distribution: List[Double],
  _parents: => List[CPTNode[_]],
  _children: => List[CPTNode[_]]) extends FiniteNode[A] {
  lazy val parents: List[CPTNode[_]] = _parents
  lazy val children: List[CPTNode[_]] = _children
  lazy val cpt: CPT[A] = new CPT(variable, distribution, parents map {_.variable})

  override def toString: String = {
    def names(nodes: List[CPTNode[_]]): String =
      nodes map {child => child.variable.name} reduceOption {_ + ", " + "" + _} match {
        case Some(str) => str
        case None => ""
      }

    def nodesString(nodes: List[CPTNode[_]]): String = nodes map {node =>
      s"CPTNode(${node.variable}, ${node.distribution}, ParentNames(${names(node.parents)}), " +
        s"ChildrenNames(${names(node.children)})"
    } reduceOption {_ + ", " + _} match {
      case Some(str) => s"List($str)"
      case None => "List()"
    }

    s"CPTNode($variable, $distribution, ${nodesString(parents)}, ${nodesString(children)})"
  }
}

object CPTNode {
  def apply[A](
    variable: FiniteRandomVariable[A],
    distribution: List[Double],
    parents: List[CPTNode[_]],
    _children: => List[CPTNode[_]]): CPTNode[A] = new CPTNode(variable, distribution, parents, _children)

  private type Unapply[A] = Option[(FiniteRandomVariable[A], List[Double], List[CPTNode[_]], List[CPTNode[_]])]
  def unapply[A](node: CPTNode[A]): Unapply[A] = Some(node.variable, node.distribution, node.parents, node.children)
}