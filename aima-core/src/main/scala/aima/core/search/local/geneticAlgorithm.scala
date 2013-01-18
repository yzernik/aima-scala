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

package aima.core.search.local

import scala.annotation._

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): Figure 4.8, page
 * 129.<br>
 * <br>
 *
 * <pre>
 * function GENETIC-ALGORITHM(population, FITNESS-FN) returns an individual
 *   inputs: population, a set of individuals
 *           FITNESS-FN, a function that measures the fitness of an individual
 *
 *   repeat
 *     new_population &lt;- empty set
 *     for i = 1 to SIZE(population) do
 *       x &lt;- RANDOM-SELECTION(population, FITNESS-FN)
 *       y &lt;- RANDOM-SELECTION(population, FITNESS-FN)
 *       child &lt;- REPRODUCE(x, y)
 *       if (small random probability) then child &lt;- MUTATE(child)
 *       add child to new_population
 *     population &lt;- new_population
 *   until some individual is fit enough, or enough time has elapsed
 *   return the best individual in population, according to FITNESS-FN
 * --------------------------------------------------------------------------------
 * function REPRODUCE(x, y) returns an individual
 *   inputs: x, y, parent individuals
 *
 *   n &lt;- LENGTH(x); c &lt;- random number from 1 to n
 *   return APPEND(SUBSTRING(x, 1, c), SUBSTRING(y, c+1, n))
 * </pre>
 *
 * Figure 4.8 A genetic algorithm. The algorithm is the same as the one
 * diagrammed in Figure 4.6, with one variation: in this more popular version,
 * each mating of two parents produces only one offspring, not two.
 *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
object geneticAlgorithm {
  type Population[I] = Seq[I]
  /**
   * Artificial Intelligence A Modern Approach (3rd Edition): page 127.<br>
   * <br>
   * Each state is rated by the objective function, or (in Genetic Algorithm
   * terminology) the fitness function. A fitness function should return higher
   * values for better states.
   */
  type Fitness[I] = I => Double
  type RandomSelection[I] = (Population[I], Fitness[I]) => I
  type Reproduce[I] = (I, I) => I
  type Mutate[I] = I => I
  type DoMutate = () => Boolean

  /**
   * @tparam I Individual - A state in a genetic algorithm is represented as an individual from the population.
   */
  def apply[I](
    population: Population[I],
    fitness: Fitness[I],
    randomSelection: RandomSelection[I],
    reproduce: Reproduce[I],
    mutate: Mutate[I],
    minFitness: Double,
    doMutate: DoMutate): I = {
    @tailrec
    def recur(best: I, currentPopulation: Population[I]): I = {
      @tailrec
      def newPop(count: Int, newPopulation: Population[I]): Population[I] =
        if (count < currentPopulation.size) {
          val (x, y) = (randomSelection(currentPopulation, fitness), randomSelection(currentPopulation, fitness))
          val child = if (doMutate()) mutate(reproduce(x, y)) else reproduce(x, y)
          newPop(count + 1, newPopulation :+ child)
        } else newPopulation
      if (fitness(best) < minFitness) {
        val newPopulation = newPop(0, currentPopulation)
        recur(newPopulation maxBy fitness, newPopulation)
      } else best
    }
    recur(population maxBy fitness, population)
  }
}
