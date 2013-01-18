/*
 * Copyright 2012, 2013 Alex DiCarlo
 *
 * This file is part of aima-scala.
 *
 * Aima-scala is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Aima-scala is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with aima-scala.  If not, see <http://www.gnu.org/licenses/>.
 */

package aima.core.logic.propositional

import aima.core.framework.Agent

trait KBAgent[A, P] extends Agent[A, P] {
  val kb: KnowledgeBase[A]
  val makePerceptSentence: (P, Int) => Sentence
  val makeActionQuery: Int => Sentence
  val makeActionSentence: (Option[A], Int) => Sentence
  private var t = 0

  /**
   * @param percept for the agent to consider
   * @return Agent that has considered new percept and action that resulted from that new percept and any previous
   *         percepts
   */
  def agentProgram(percept: P): Option[A] = {
    kb.tell(makePerceptSentence(percept, t))
    val action = kb.ask(makeActionQuery(t))
    kb.tell(makeActionSentence(action, t))
    t = t + 1
    action
  }
}
