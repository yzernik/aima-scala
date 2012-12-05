package aima.core.search.nondeterministic

import aima.core.search._

/**
 * Non-deterministic problems may have multiple results for a given state and
 * action; this class handles these results by mimicking Problem and replacing
 * ResultFunction (one result) with ResultsFunction (a set of results).
 *
 * Author: dicarlo2 (Alex)
 * Date: 11/20/12
 */
case class NDProblem[S, A](
  initialState: S,
  actions: Actions[S, A],
  results: Results[S, A],
  goalTest: GoalTest[S],
  stepCost: StepCost[S, A])
