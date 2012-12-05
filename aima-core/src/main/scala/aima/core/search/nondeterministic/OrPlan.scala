package aima.core.search.nondeterministic

/**
 * Represents a solution plan for an AND-OR search; according to page 135
 * AIMA3e, the plan must be "a subtree that (1) has a goal node at every leaf,
 * (2) specifies one Object at each of its OR nodes, and (3) includes every
 * outcome branch at each of its AND nodes." As demonstrated on page 136, this
 * subtree is implemented as a linked list where every OR node is an Object--
 * satisfying (2)--and every AND node is an if-state-then-plan-else
 * chain--satisfying (3).
 *
 * <b>Note:</b> Specifically, at an OR node in the tree we choose one action which leads to a state where the
 * environment "chooses" a state for us, leading to an AndPlan that reacts to that state. A solution produced by
 * andOrGraphSearch will never throw an IllegalArgumentException, because we will never reach an invalid state
 * (otherwise the search would have failed in the first place).
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
final class OrPlan[S, A](state: S, action: Option[A], andPlan: Option[AndPlan[S, A]]) {
  def apply(state: S): (Option[A], Option[AndPlan[S, A]]) =
    if (state == this.state)
      (action, andPlan)
    else
      throw new IllegalArgumentException("Invalid state for OrPlan")
}
