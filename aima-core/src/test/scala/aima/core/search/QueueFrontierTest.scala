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

import scala.collection.immutable.Queue

class QueueFrontierTest extends FrontierTest {
  type Collection = Queue[Node[String, Nothing]]
  protected def name: String = "QueueFrontier"
  protected def collectionName: String = "Queue"
  protected def createFrontier(): Frontier[String, Nothing] = QueueFrontier()
  protected def createCollection(): Queue[Node[String, Nothing]] = Queue()
  protected def addToCollection(
    collection: Queue[Node[String, Nothing]],
    element: Node[String, Nothing]): Queue[Node[String, Nothing]] = collection enqueue element
  protected def firstOfCollection(collection: Queue[Node[String, Nothing]]): Node[String, Nothing] =
    collection.head
  protected def restOfCollection(collection: Queue[Node[String, Nothing]]): Queue[Node[String, Nothing]] =
    collection.dequeue._2
}
