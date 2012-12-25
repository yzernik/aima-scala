package aima.core.logic.fol

trait KnowledgeBase {
  def tell[T <: Sentence](sentence: T): KnowledgeBase = store(sentence)
  def tell[T <: Sentence](sentences: Set[T]): KnowledgeBase = (sentences foldLeft this) {case (kb, s) => kb.tell(s)}
  protected def store(sentence: Sentence): KnowledgeBase
  def ask(sentence: Sentence): InferenceResult
  def fetch(literal: Literal): Set[Substitution]
  def fetch(literals: List[Literal]): Set[Substitution]
  def implicationDefinites: Set[ImplicationDefinite]
}
