package aima.core.search.nondeterministic

/**
 * Represents an if-state-then-plan statement for use with AND-OR search;
 * explanation given on page 135 of AIMA3e.
 *
 * <b>Note:</b> Specifically, at an AND node in the tree, the state is chosen for the agent by the environment so it
 * must choose a contingency plan, an OrPlan, that reacts to the state and chooses an action. Similar to an OrPlan,
 * on a successful result from andOrGraphSearch, an IllegalArgumentException will never be thrown.
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
final class AndPlan[S, A](stateTable: Map[S, OrPlan[S, A]]) {
  def apply(state: S): OrPlan[S, A] =
    stateTable.getOrElse(state, throw new IllegalArgumentException("Invalid state for AndPlan"))
}
