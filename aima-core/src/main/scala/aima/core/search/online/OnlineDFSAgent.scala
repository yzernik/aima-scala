package aima.core.search.online

import aima.core.framework.Agent

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.21, page
 * 150.<br>
 * <br>
 *
 * <pre>
 * function ONLINE-DFS-AGENT(s') returns an action
 *   inputs: s', a percept that identifies the current state
 *   persistent: result, a table, indexed by state and action, initially empty
 *               untried, a table that lists, for each state, the actions not yet tried
 *               unbacktracked, a table that lists, for each state, the backtracks not yet tried
 *               s, a, the previous state and action, initially null
 *
 *   if GOAL-TEST(s') then return stop
 *   if s' is a new state (not in untried) then untried[s'] &lt;- ACTIONS(s')
 *   if s is not null then
 *       result[s, a] &lt;- s'
 *       add s to the front of the unbacktracked[s']
 *   if untried[s'] is empty then
 *       if unbacktracked[s'] is empty then return stop
 *       else a &lt;- an action b such that result[s', b] = POP(unbacktracked[s'])
 *   else a &lt;- POP(untried[s'])
 *   s &lt;- s'
 *   return a
 * </pre>
 *
 * Figure 4.21 An online search agent that uses depth-first exploration. The
 * agent is applicable only in state spaces in which every action can be
 * "undone" by some other action.<br>
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/22/12
 */
final class OnlineDFSAgent[S, A, P](perceptMapper: PerceptMapper[P, S], problem: OnlineSearchProblem[S, A])
  extends Agent[A, P] {
  private var result = Map[(S, A), S]()
  private var untried = Map[S, Seq[A]]()
  private var unbacktracked = Map[S, Seq[S]]()
  private var a: Option[A] = None
  private var s: Option[S] = None

  /**
   *
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = {
    def findBacktrackAction(state: S): Option[A] = {
      val backtrackState = unbacktracked.get(state).get.head
      problem.actions(state) find { action =>
        result.get((state, action)) match {
          case Some(resultState) if resultState == backtrackState => true
          case _ => false
        }
      }
    }

    val sPrime = perceptMapper(percept)
    if (problem.goalTest(sPrime))
      return None
    if (!(untried isDefinedAt sPrime))
      untried += sPrime → problem.actions(sPrime)
    if (s.isDefined) {
      result += (s.get, a.get) → sPrime
      unbacktracked += sPrime → (unbacktracked.getOrElse(sPrime, Seq()) :+ s.get)
    }
    if (untried get sPrime map { _.isEmpty } getOrElse true) {
      if (unbacktracked get sPrime map { _.isEmpty } getOrElse true)
        return None
      else
        a = findBacktrackAction(sPrime)
    } else {
      a = Some(untried.get(sPrime).get.head)
      untried += sPrime → untried.get(sPrime).get.tail
    }
    s = Some(sPrime)
    a
  }

}