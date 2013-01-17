package aima.core.search

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{GivenWhenThen, FeatureSpec}
import scala.collection.immutable.Queue

class searchTest extends FeatureSpec with
                         GivenWhenThen with
                         ShouldMatchers with
                         GeneratorDrivenPropertyChecks with
                         MockFactory {
  abstract class StringAction(val addition: String)
  case object AddA extends StringAction("A")
  case object AddB extends StringAction("B")
  case object AddC extends StringAction("C")

  def actions(state: String): Seq[StringAction] =
    if (state.length < 2) Seq(AddA, AddB, AddC) else Seq()
  def result(state: String, action: StringAction): String = state + action.addition
  def goalTest(state: String): Boolean = state == "B"
  def stepCost(s: String, action: StringAction, statePrime: String): Int = 1
  val stringProblem = Problem("", actions, result, goalTest, stepCost)

  def childNode(parent: Node[String, StringAction], action: StringAction): Node[String, StringAction] =
    Node.createChildNode(stringProblem, parent, action)
  def children(parent: Node[String, StringAction]): Seq[Node[String, StringAction]] =
    Seq(childNode(parent, AddA), childNode(parent, AddB), childNode(parent, AddC))
  def nodeExpander(): NodeExpander[String, StringAction] = {
    val nodeExpander = mock[NodeExpander[String, StringAction]]
    inSequence {
      val initialNode = Node[String, StringAction](stringProblem.initialState, None, None, 0.0)
      val addAChild = childNode(initialNode, AddA)
      val addBChild = childNode(initialNode, AddB)
      (nodeExpander.apply _).expects(stringProblem, initialNode)
        .returning(Seq(addAChild, addBChild, childNode(initialNode, AddC)), nodeExpander)
      (nodeExpander.apply _).expects(stringProblem, addAChild).returning(children(addAChild), nodeExpander)
      (nodeExpander.apply _).expects(stringProblem, addBChild).returning(children(addBChild), nodeExpander)
    }
    nodeExpander
  }

  feature("Searching with a node expander") {
    scenario("Searching with a node expander") {
      Given("a frontier")
      val frontier: Frontier[String, StringAction] = List()
      And("a node expander")
      val nodeExpander = nodeExpander()
      And("a problem")
      When("#search() is called with the node expander, a frontier, and a problem")
      search(frontier, nodeExpander)(stringProblem)
      Then("the node expander is used to expand nodes")
    }

    scenario("Searching with a frontier") {
      Given("a frontier")
      val frontier: Frontier[String, StringAction] = Queue()
      And("a node expander")
      val nodeExpander = mock[NodeExpander[String, StringAction]]
      inSequence {
        val initialNode = Node[String, StringAction](stringProblem.initialState, None, None, 0.0)
        val addAChild = childNode(initialNode, AddA)
        val addBChild = childNode(initialNode, AddB)
        (nodeExpander.apply _).expects(stringProblem, initialNode)
          .returning(Seq(addAChild, addBChild, childNode(initialNode, AddC)), nodeExpander)
        val addAChildren = children(addAChild)
        (nodeExpander.apply _).expects(stringProblem, addAChild).returning(addAChildren, nodeExpander)
        (nodeExpander.apply _).expects(stringProblem, addAChildren(0)).returning(Seq(), nodeExpander)
        (nodeExpander.apply _).expects(stringProblem, addAChildren(1)).returning(Seq(), nodeExpander)
        (nodeExpander.apply _).expects(stringProblem, addAChildren(2)).returning(Seq(), nodeExpander)
        (nodeExpander.apply _).expects(stringProblem, addBChild).returning(children(addBChild), nodeExpander)
      }
      nodeExpander
      And("a problem")
      When("#search() is called with the node expander, a frontier, and a problem")
      search(frontier, nodeExpander)(stringProblem)
      Then("expanded nodes are added to the frontier")
      And("the previous node is removed from the frontier")

    }

    scenario("Solving a Problem through search") {
      Given("a frontier")
      val frontier = List()
      And("a node expander")
      val nodeExpander = nodeExpander()
      When("#search() is called with the node expander, a frontier, and a problem")
      val result = search(frontier, nodeExpander)(stringProblem)
      Then("the result is Success(Seq(AddB))")
      result should be ('defined)
      result.get should equal(Seq(AddB))
    }
  }
}
