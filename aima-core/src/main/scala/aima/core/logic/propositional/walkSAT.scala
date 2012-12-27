package aima.core.logic.propositional

import scala.util.Random._
import scala.annotation.tailrec

object walkSAT {
  def apply(clauses: Set[Clause], p: Double, maxFlips: Int): Option[Model] = {
    @tailrec
    def recur(model: Model, iter: Seq[Int]): Option[Model] = {
      if (clauses forall {_ isTrueIn model}) Some(model) else iter match {
        case _ :: tail =>
          val symbols = shuffle(clauses filterNot {_ isTrueIn model}).head.literals flatMap {_.symbols}
          val flippedSymbol = if (nextDouble() < p)
                                shuffle(symbols).head
                              else symbols maxBy
                                {symbol => clauses count {_ isTrueIn model + (symbol → !model.get(symbol).get)}}
          recur(model + (flippedSymbol → model.get(flippedSymbol).get), tail)
        case Nil => None
      }
    }
    val model = (clauses flatMap {_.symbols} map {(_, if (nextDouble() < 0.5) true else false)}).toMap
    recur(model, 1 to maxFlips)
  }
}
