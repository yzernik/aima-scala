package aima.core.search

/**
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
package object nondeterministic {
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 136.<br>
   * <br>
   * Closely related to ResultFunction, but for non-deterministic problems; in
   * these problems, the outcome of an action will be a set of results, not a
   * single result. This class implements the functionality of RESULTS(s, a), page
   * 136, returning the states resulting from doing action a in state s.
   */
  type Results[S, A] = (S, A) => Seq[S]
}
