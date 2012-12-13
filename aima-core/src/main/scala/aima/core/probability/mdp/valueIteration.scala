package aima.core.probability.mdp

import scala.annotation.tailrec

object valueIteration {
  def apply[S, A](mdp: MarkovDecisionProcess[S, A], gamma: Double, epsilon: Double): Map[S, Double] = {
    @tailrec
    def recur(utilities: Map[S, Double], delta: Double): Map[S, Double] = {
      if (delta < epsilon * (1 - gamma) / gamma) {
        utilities
      } else {
        val nextUtilities = mdp.states map {
          state => {
            val utility = (mdp rewardIn state) + gamma * (mdp actionsIn state map {
              action => mdp.states.foldLeft(0.0) {
                (sum, statePrime) => sum + mdp.transition(statePrime, state, action) * utilities(statePrime)
              }
            }).max
            (state, utility)
          }
        }
        val nextDelta = (nextUtilities map {case (state, util) => math.abs(util - utilities(state))}).max
        recur(nextUtilities.toMap, nextDelta)
      }
    }
    recur(Map().withDefault(s => 0.0), Double.MaxValue)
  }
}
