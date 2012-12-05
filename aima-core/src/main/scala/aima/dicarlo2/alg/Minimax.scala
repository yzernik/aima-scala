package edu.uiuc.cs.dicarlo2.alg

object Minimax {
  // returns (best action, number of nodes expanded)
  def apply(cutoff: Cutoff)(utility: Utility)(state: State): (Action, Int) = {
    val (action, (_, cnt)) = actions(state).par
      .map(action => (action, minValue(result(state, action), 1)(cutoff, utility)))
      .reduce[(Action, (Int, Int))] {
      case ((action1, (val1, cnt1)), (action2, (val2, cnt2))) =>
        if (val1 > val2) (action1, (val1, cnt1 + cnt2)) else (action2, (val2, cnt1 + cnt2))
    }
    (action, cnt + 1)
  }

  private def maxValue(state: State, d: Int)(implicit cutoff: Cutoff, utility: Utility): (Int, Int) = {
    if (cutoff(state, d))
      (utility(state), 1)
    else {
      val mapResult = actions(state).map(action => minValue(result(state, action), d + 1))
      (mapResult.maxBy(_._1)._1, (mapResult :\ 1) { case ((_, cnt), cntSoFar) => cntSoFar + cnt })
    }
  }

  private def minValue(state: State, d: Int)(implicit cutoff: Cutoff, utility: Utility): (Int, Int) = {
    if (cutoff(state, d)) {
      val res = (utility(state), 1)
      res
    }
    else {
      val mapResult = actions(state).map(action => maxValue(result(state, action), d + 1))
      (mapResult.minBy(_._1)._1, (mapResult :\ 1) { case ((_, cnt), cntSoFar) => cntSoFar + cnt })
    }
  }
}
