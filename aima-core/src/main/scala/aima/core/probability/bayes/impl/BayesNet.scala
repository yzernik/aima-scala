package aima.core.probability.bayes.impl

import aima.core.probability.RandomVariable
import aima.core.probability.bayes.{Node, BayesianNetwork}
import scala.Some
import scala.annotation._

final case class BayesNet(rootNodes: Node[_]*) extends BayesianNetwork {
  val variables: List[RandomVariable[_]] = _variables
  private val variableNodeMap: Map[RandomVariable[_], Node[_]] = _variableNodeMap

  def nodeFor[A](variable: RandomVariable[A]): Option[Node[A]] =
    (variableNodeMap get variable).asInstanceOf[Option[Node[A]]]

  private lazy val (_variables, _variableNodeMap): (List[RandomVariable[_]], Map[RandomVariable[_], Node[_]]) = {
    type Nodes = List[Node[_]]
    type NodeSet = Set[Node[_]]
    type Edges = Map[Node[_], Set[Node[_]]]
    @tailrec
    def walkNodes(nodes: Nodes, visited: NodeSet, incomingEdges: Edges): (NodeSet, Edges) = nodes match {
      case node :: tail if visited(node) => walkNodes(tail, visited, incomingEdges)
      case node :: tail => walkNodes(tail ++ node.children, visited + node, incomingEdges + (node → node.parents.toSet))
      case Nil => (visited, incomingEdges)
    }

    @tailrec
    def addIncoming(n: Node[_], children: Nodes, noInc: NodeSet, incEdges: Edges): (NodeSet, Edges) = children match {
      case child :: tail =>
        val nextNoIncoming = if ((incEdges(child) - n).isEmpty) noInc + child else noInc
        addIncoming(n, tail, nextNoIncoming, incEdges + (child → (incEdges(child) - n)))
      case Nil => (noInc, incEdges)
    }

    @tailrec
    def recur(noIncoming: NodeSet, incomingEdges: Edges, sorted: Nodes): Nodes = noIncoming.headOption match {
      case Some(node) =>
        val (nextNoIncoming, nextIncomingEdges) = addIncoming(node, node.children, noIncoming - node, incomingEdges)
        recur(nextNoIncoming, nextIncomingEdges, sorted :+ node)
      case None if incomingEdges.valuesIterator forall {_.isEmpty} => sorted
      case _ => throw new IllegalArgumentException("Network contains at least one cycle in it, must be a DAG.")
    }

    val topoNodes = recur(rootNodes.toSet, walkNodes(rootNodes.toList, Set(), Map())._2, List())
    (topoNodes map {_.variable}, topoNodes.map(n => n.variable → n).toMap)
  }
}


