package aima.core.agent

import aima.core.framework._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 2.7, page 47.<br>
 * <br>
 *
 * <pre>
 * function TABLE-DRIVEN-AGENT(percept) returns an action
 *   persistent: percepts, a sequence, initially empty
 *               table, a table of actions, indexed by percept sequences, initially fully specified
 *
 *   append percept to end of percepts
 *   action <- LOOKUP(percepts, table)
 *   return action
 * </pre>
 *
 * Figure 2.7 The TABLE-DRIVEN-AGENT program is invoked for each new percept and
 * returns an action each time. It retains the complete percept sequence in
 * memory.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/19/12
 */
final class TableDrivenAgent[A, P](table: Map[Seq[P], A]) extends Agent[A, P] {
  private var percepts: Seq[P] = Seq()

  /**
   *
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  override def agentProgram(percept: P): Option[A] = {
    percepts +:= percept
    table get percepts
  }
}
