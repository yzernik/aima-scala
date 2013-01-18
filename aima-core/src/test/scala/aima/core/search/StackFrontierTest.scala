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

import scala.collection.immutable.Stack

class StackFrontierTest extends FrontierTest {
  type Collection = Stack[Node[String, Nothing]]
  protected def name: String = "StackFrontier"
  protected def collectionName: String = "Stack"
  protected def createFrontier(): Frontier[String, Nothing] = StackFrontier()
  protected def createCollection(): Stack[Node[String, Nothing]] = Stack()
  protected def addToCollection(
    collection: Stack[Node[String, Nothing]],
    element: Node[String, Nothing]): Stack[Node[String, Nothing]] = collection push element
  protected def firstOfCollection(collection: Stack[Node[String, Nothing]]): Node[String, Nothing] =
    collection.top
  protected def restOfCollection(collection: Stack[Node[String, Nothing]]): Stack[Node[String, Nothing]] =
    collection.pop
}
