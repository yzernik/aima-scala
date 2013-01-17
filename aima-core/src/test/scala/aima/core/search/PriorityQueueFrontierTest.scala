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

import scala.collection.mutable

class PriorityQueueFrontierTest extends FrontierTest {
  type Collection = mutable.PriorityQueue[Node[String, Nothing]]
  protected def name: String = "PriorityQueueFrontier"
  protected def collectionName: String = "PriorityQueue"
  private val ordering: Ordering[Node[String, Nothing]] = Ordering.by(_ => 0)
  protected def createFrontier(): Frontier[String, Nothing] = PriorityQueueFrontier[String, Nothing](ordering)
  protected def createCollection(): mutable.PriorityQueue[Node[String, Nothing]] = mutable.PriorityQueue()(ordering)
  protected def addToCollection(
    collection: mutable.PriorityQueue[Node[String, Nothing]],
    element: Node[String, Nothing]): mutable.PriorityQueue[Node[String, Nothing]] = {
    collection enqueue element
    collection
  }
  protected def firstOfCollection(collection: mutable.PriorityQueue[Node[String, Nothing]]): Node[String, Nothing] =
    collection.head
  protected def restOfCollection(collection: mutable.PriorityQueue[Node[String, Nothing]]): mutable.PriorityQueue[Node[String, Nothing]] = {
    collection.dequeue()
    collection
  }
}