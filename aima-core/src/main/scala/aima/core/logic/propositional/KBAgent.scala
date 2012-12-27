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
