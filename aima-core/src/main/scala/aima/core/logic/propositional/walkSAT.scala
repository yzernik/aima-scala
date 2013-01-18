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
