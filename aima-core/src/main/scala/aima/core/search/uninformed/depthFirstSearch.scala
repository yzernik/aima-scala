package aima.core.search.uninformed

import aima.core.search._
import scala.collection.immutable

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): page 85.<br>
 * <br>
 * Depth-first search always expands the deepest node in the current frontier of
 * the search tree. <br>
 * <br>
 * <b>Note:</b> Supports both Tree and Graph based versions by using graphSearch or
 * treeSearch as an argument
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/21/12
 */
object depthFirstSearch {
  def apply[S, A](search: FrontierSearch[S, A]): Search[S, A, Seq[A]] =
    search(immutable.Stack[Node[S, A]]())
}
