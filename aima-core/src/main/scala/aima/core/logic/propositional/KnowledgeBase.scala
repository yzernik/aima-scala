package aima.core.logic.propositional

trait KnowledgeBase[A] {
  def tell(sentence: Sentence): KnowledgeBase[A]

  def ask(sentence: Sentence): Option[A]

  def asSentence: Sentence
}
