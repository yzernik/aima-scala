package aima.core.search

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 3.10, page
 * 79.<br>
 *
 * Figure 3.10 Nodes are the data structures from which the search tree is
 * constructed. Each has a parent, a state, and various bookkeeping fields.
 * Arrows point from child to parent.<br>
 * <br>
 * Search algorithms require a data structure to keep track of the search tree
 * that is being constructed. For each node n of the tree, we have a structure
 * that contains four components:
 * <ul>
 * <li>n.STATE: the state in the state space to which the node corresponds;</li>
 * <li>n.PARENT: the node in the search tree that generated this node;</li>
 * <li>n.ACTION: the action that was applied to the parent to generate the node;
 * </li>
 * <li>n.PATH-COST: the cost, traditionally denoted by g(n), of the path from
 * the initial state to the node, as indicated by the parent pointers.</li>
 * </ul>
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
class Node[S, A](val state: S, val parent: Option[Node[S, A]], val action: Option[A], val pathCost: Double) {
  lazy val pathToRoot: Seq[Node[S, A]] = parent.foldLeft(Seq(this))((seq, parent) => parent.pathToRoot ++ seq)

  override def equals(other: Any): Boolean = other match {
    case that: Node[S,A] => (that canEqual this) && state == that.state
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node[S, A]]

  override def hashCode(): Int = state.hashCode()
}

object Node {
  def apply[S, A](state: S, parent: Option[Node[S, A]], action: Option[A], pathCost: Double): Node[S, A] =
    new Node[S, A](state, parent, action, pathCost)

  def createChildNode[S, A](problem: Problem[S, A], parent: Node[S, A], action: A): Node[S, A] = {
    val state = problem.result(parent.state, action)
    val pathCost = parent.pathCost + problem.stepCost(parent.state, action, state)
    Node(state, Some(parent), Some(action), pathCost)
  }
}