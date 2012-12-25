package aima.core.logic.fol

trait KnowledgeBase {
  def tell(sentence: Sentence): KnowledgeBase = store(sentence)
  def tell(sentences: Set[Sentence]): KnowledgeBase = (sentences foldLeft this) {case (kb, s) => kb.tell(s)}
  protected def store(sentence: Sentence): KnowledgeBase
  def ask(sentence: Sentence): InferenceResult
  def fetch(literal: Literal): Set[Substitution]
  def fetch(literals: List[Literal]): Set[Substitution]
}
