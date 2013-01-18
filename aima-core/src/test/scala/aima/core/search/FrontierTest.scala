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

import org.scalacheck.Gen
import org.scalatest.{GivenWhenThen, FeatureSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

trait FrontierTest extends FeatureSpec with
                           GivenWhenThen with
                           ShouldMatchers with
                           GeneratorDrivenPropertyChecks {
  type Collection <: Traversable[Node[String, Nothing]]
  protected def name: String
  protected def collectionName: String
  protected def createFrontier(): Frontier[String, Nothing]
  protected def createCollection(): Collection
  protected def addToCollection(
    collection: Collection,
    element: Node[String, Nothing]): Collection
  protected def firstOfCollection(collection: Collection): Node[String, Nothing]
  protected def restOfCollection(collection: Collection): Collection

  private val elements = for {
    state <- Gen.alphaStr
  } yield Node(state, None, None, 0.0)

  feature(s"$name works like a $collectionName") {
    scenario(s"$name#add adds an element") {
      Given(s"a $name")
      var frontier = createFrontier()
      And("an element")
      val element = elements.sample.get
      When(s"I add the element to the $name")
      frontier = frontier add element
      Then(s"$name#headOption should give me that element")
      frontier.headOption should be(Some(element))
    }

    scenario(s"$name#addAll pushes all elements") {
      Given(s"a $name")
      val frontier = createFrontier()
      And("some elements")
      val elementLists = for {
        lists <- Gen.containerOf[List, Node[String, Nothing]](elements)
      } yield lists
      When(s"I add the elements to the $name")
      forAll(elementLists) {elementList =>
        val newStackFrontier = frontier addAll elementList
        elementList foreach {ele => newStackFrontier should contain(ele)}
      }
      Then(s"the $name should have all of the elements")
    }

    scenario(s"$name#rest removes an element") {
      Given(s"a $name with one element")
      var frontier = createFrontier()
      val element = elements.sample.get
      frontier = frontier add element
      When("I call #rest")
      frontier = frontier.rest
      Then(s"the resulting $name should have no elements")
      frontier.size should be(0)
    }

    scenario(s"$name works identically to a $collectionName when the same operations are applied") {
      Given(s"a $name")
      var stackFrontier = createFrontier()
      And(s"a $collectionName")
      var collection = createCollection()
      And("randomly generated nodes")
      When(s"I add the elements to the $collectionName and $name")
      And("then remove them, they work identically")

      forAll(elements) {element =>
        stackFrontier = stackFrontier add element
        collection = addToCollection(collection, element)
      }

      while (!collection.isEmpty) {
        val frontierElement = stackFrontier.headOption.get
        val stackElement = firstOfCollection(collection)
        frontierElement should be(stackElement)
        stackFrontier = stackFrontier.rest
        collection = restOfCollection(collection)
      }
      stackFrontier should be('empty)
    }
  }
}
