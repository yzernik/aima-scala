package aima.core

import aima.core.agent._

/**
 * Author: dicarlo2 (Alex)
 * Date: 11/19/12
 */
package object search {
  /**
   * This function is to define how to Map a Percept to a State representation
   * for a problem solver within a specific environment. This arises in the
   * description of the Online Search algorithms from Chapter 4.
   */
  type UpdateState[S, P] = (S, P) => S
  type FormulateGoal[S] = S => S
  type FormulateProblem[S, A] = (S, S) => Problem[S, A]
  type Search[S, A, T] = Problem[S, A] => SearchResult[T]
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 67.<br>
   * <br>
   * Given a particular state s, ACTIONS(s) returns the set of actions that can be
   * executed in s. We say that each of these actions is <b>applicable</b> in s.
   */
  type Actions[S, A] = S => Seq[A]

  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 67.<br>
   * <br>
   * A description of what each action does; the formal name for this is the
   * transition model, specified by a function RESULT(s, a) that returns the state
   * that results from doing action a in state s. We also use the term successor
   * to refer to any state reachable from a given state by a single action.
   */
  type Result[S, A] = (S, A) => S

  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 67.<br>
   * <br>
   * The goal test, which determines whether a given state is a goal state.
   */
  type GoalTest[S] = S => Boolean

  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 68.<br>
   * <br>
   * The <b>step cost</b> of taking action a in state s to reach state s' is
   * denoted by c(s, a, s').
   */
  type StepCost[S, A] = (S, A, S) => Double
  type Frontier[S, A] = Traversable[Node[S, A]]
  type NodeExpander[S, A] = (Problem[S, A], Node[S, A]) => Seq[Node[S, A]]
  type FrontierExpander[S, A] = (Problem[S, A], Node[S, A], Frontier[S, A]) => Frontier[S, A]
  type FrontierSearch[S, A] = Frontier[S, A] => Search[S, A, Seq[A]]
  type Explored[S, A] = Set[Node[S, A]]
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 92.<br>
   * <br>
   * a heuristic function, denoted h(n):<br>
   * h(n) = estimated cost of the cheapest path from the state at node n to a goal
   * state.<br>
   * <br>
   * Notice that h(n) takes a node as input, but, unlike g(n) it depends only on
   * the state at that node.
   */
  type Heuristic[S] = S => Double

  def solutionActions[S, A](node: Node[S, A]): Seq[A] =
    node.pathToRoot withFilter { _.action.isEmpty } map { _.action.get }
}
