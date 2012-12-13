package aima.core.probability.mdp

trait MarkovDecisionProcess[S, A] {
  def initialState: S
  def states: Seq[S]
  def actionsIn(state: S): Seq[A]
  def transition(statePrime: S, state: S, action: A): Double
  def rewardIn(state: S): Double
}

object MarkovDecisionProcess {
  type Actions[S, A] = S => Seq[A]
  type TransitionModel[S, A] = (S, S, A) => Double
  type Reward[S] = S => Double

  def apply[S, A](
    aInitialState: S,
    aStates: Seq[S],
    aActions: Actions[S, A],
    aTransitionModel: TransitionModel[S, A],
    aReward: Reward[S]): MarkovDecisionProcess[S, A] = new MarkovDecisionProcess[S, A] {
    val initialState: S = aInitialState
    val states: Seq[S] = aStates
    def actionsIn(state: S): Seq[A] = aActions(state)
    def transition(statePrime: S, state: S, action: A): Double = aTransitionModel(statePrime, state, action)
    def rewardIn(state: S): Double = aReward(state)
  }
}


