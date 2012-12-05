package edu.uiuc.cs.dicarlo2.alg

import Math.{max, min}

object AlphaBetaSearch {
  // returns (best action, number of nodes expanded)
  def apply(cutoff: Cutoff)(utility: Utility)(state: State): (Action, Int) = {
    val (action, _, cnt) = actions(state).par.map(action => {
      val (v, cnt) = minValue(result(state, action), Int.MinValue, Int.MaxValue, 1)(cutoff, utility)
      (action, v, cnt)
    }).reduce[(Action, Int, Int)]({ case ((action1, val1, cnt1), (action2, val2, cnt2)) =>
      if (val1 > val2) (action1, val1, cnt1 + cnt2) else (action2, val2, cnt1 + cnt2)
    })
    (action, cnt + 1)
  }

  private def maxValue(state: State, α: Int, β: Int, d: Int)(implicit cutoff: Cutoff, utility: Utility): (Int, Int) = {
    if (cutoff(state, d))
      (utility(state), 1)
    else {
      var v = Int.MinValue
      val (_, cnt) = actions(state).toStream.takeWhile(_ => v < β).foldLeft((α, 1))({ case ((α, cnt), action) =>
        val (actionVal, actionCnt) = minValue(result(state, action), α, β, d + 1)
        v = max(v, actionVal)
        (max(α, v), cnt + actionCnt)
      })
      (v, cnt)
    }
  }

  private def minValue(state: State, α: Int, β: Int, d: Int)(implicit cutoff: Cutoff, utility: Utility): (Int, Int) = {
    if (cutoff(state, d))
      (utility(state), 1)
    else {
      var v = Int.MaxValue
      val (_, cnt) = actions(state).toStream.takeWhile(_ => v > α).foldLeft((β, 1))({ case ((β, cnt), action) =>
        val (actionVal, actionCnt) = maxValue(result(state, action), α, β, d + 1)
        v = min(v, actionVal)
        (min(β, v), cnt + actionCnt)
      })
      (v, cnt)
    }
  }
}
