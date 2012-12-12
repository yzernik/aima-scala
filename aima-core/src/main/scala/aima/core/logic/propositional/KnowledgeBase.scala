package aima.core.logic.propositional

trait KnowledgeBase[A] extends Sentence {
  def tell(sentence: Sentence): KnowledgeBase[A]

  def ask(sentence: Sentence): Option[A]
}
