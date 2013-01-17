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

package aima.core.search

import scala.collection.immutable.{Queue, Stack}
import scala.collection.mutable

trait Frontier[S, A] extends Traversable[Node[S, A]] {
  def first: Option[Node[S, A]]
  def rest: Frontier[S, A]
  def add(node: Node[S, A]): Frontier[S, A]
  def addAll(trav: Traversable[Node[S, A]]): Frontier[S, A] =
    (trav foldLeft this) {case (frontier, node) => frontier add node}
}

object StackFrontier {
  private final class StackFrontier[S, A](stack: Stack[Node[S, A]]) extends Frontier[S, A] {
    def first: Option[Node[S, A]] = stack.headOption
    def rest: Frontier[S, A] = new StackFrontier(stack pop)
    def foreach[U](f: (Node[S, A]) => U) {stack foreach f}
    def add(node: Node[S, A]): Frontier[S, A] = new StackFrontier(stack push node)
  }

  def apply[S, A](): Frontier[S, A] = new StackFrontier(Stack[Node[S, A]]())
}

object QueueFrontier {
  private final class QueueFrontier[S, A](queue: Queue[Node[S, A]]) extends Frontier[S, A] {
    def first: Option[Node[S, A]] = queue.headOption
    def rest: Frontier[S, A] = new QueueFrontier(queue.dequeue._2)
    def foreach[U](f: (Node[S, A]) => U) {queue foreach f}
    def add(node: Node[S, A]): Frontier[S, A] = new QueueFrontier(queue enqueue node)
  }

  def apply[S, A](): Frontier[S, A] = new QueueFrontier(Queue[Node[S, A]]())
}

object PriorityQueueFrontier {
  private final class PriorityQueueFrontier[S, A](queue: mutable.PriorityQueue[Node[S, A]]) extends Frontier[S, A] {
    def first: Option[Node[S, A]] = queue.headOption
    def rest: Frontier[S, A] = {
      queue.dequeue()
      this
    }
    def foreach[U](f: (Node[S, A]) => U) {queue foreach f}
    def add(node: Node[S, A]): Frontier[S, A] = {
      queue enqueue node
      this
    }
  }

  def apply[S, A](order: Ordering[Node[S, A]]): Frontier[S, A] =
    new PriorityQueueFrontier(mutable.PriorityQueue[Node[S, A]]()(order))
}
