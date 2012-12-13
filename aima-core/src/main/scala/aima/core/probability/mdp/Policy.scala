package aima.core.probability.mdp

import scala.collection.MapProxy

trait Policy[S, A] {
  def action(state: S): A
}

case class MapPolicy[S, A](actionMap: Map[S, A]) extends Policy[S, A] with MapProxy[S, A] {
  def action(state: S): A = actionMap(state)
  def self = actionMap
}

object MapPolicy {
  def convert[S, A](policy: Policy[S, A], mdp: MarkovDecisionProcess[S, A]): MapPolicy[S, A] =
    MapPolicy(mdp.states.map(state => (state, policy.action(state)))(collection.breakOut))
}