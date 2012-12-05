package aima.core.search.online

import aima.core.search._

 /**
  * Artificial Intelligence A Modern Approach (3rd Edition): page 147.<br>
  * <br>
  * An online search problem must be solved by an agent executing actions, rather
  * than by pure computation. We assume a deterministic and fully observable
  * environment (Chapter 17 relaxes these assumptions), but we stipulate that the
  * agent knows only the following: <br>
  * <ul>
  * <li>ACTIONS(s), which returns a list of actions allowed in state s;</li>
  * <li>The step-cost function c(s, a, s') - note that this cannot be used until
  * the agent knows that s' is the outcome; and</li>
  * <li>GOAL-TEST(s).</li>
  * </ul>
  *
 * Author: Alex DiCarlo (dicarlo2)
 * Date: 11/20/12
 */
case class OnlineSearchProblem[S, A](
  initialState: S,
  actions: Actions[S, A],
  goalTest: GoalTest[S],
  stepCost: StepCost[S, A])
