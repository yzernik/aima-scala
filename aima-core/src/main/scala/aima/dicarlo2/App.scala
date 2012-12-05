package edu.uiuc.cs.dicarlo2

import java.io._
import edu.uiuc.cs.dicarlo2.alg._
import edu.uiuc.cs.dicarlo2.Printer.printBoard

object App {
  def main(args: Array[String]) {
    val boardList = List(("/home/alex/code/school/cs440/mp2/Kalamazoo.txt", "Kalamazoo"),
      ("/home/alex/code/school/cs440/mp2/Peoria.txt", "Peoria"),
      ("/home/alex/code/school/cs440/mp2/Piqua.txt", "Piqua"),
      ("/home/alex/code/school/cs440/mp2/Punxsutawney.txt", "Punxsutawney"),
      ("/home/alex/code/school/cs440/mp2/Wallawalla.txt", "Wallawalla"))

    def runScenarioSet(scenarioSet: Traversable[Scenario]) {
      boardList.foreach{case (fileName, boardName) => {
        val board: Board = Parser.parse(new BufferedReader(new FileReader(fileName)))
        printNewBoard(board, boardName)
        scenarioSet.foreach {
          case Scenario(p1Utility, p1Cutoff, p1Agent, p2Utility, p2Cutoff, p2Agent) => {
            runGame(board, boardName)(p1Utility, p1Cutoff)(p1Agent)(p2Utility, p2Cutoff)(p2Agent)
          }
        }
      }}
    }

    printHeader("Required Scenarios")
    runScenarioSet(reqScenarios)
    printFooter("Required Scenarios")

    printHeader("Score difference vs. state score scenarios. Depth 4")
    runScenarioSet(depth4ScoreVsDiffScenarios)
    printFooter("Score difference vs. state score scenarios. Depth 4")

    printHeader("Score difference vs. state score scenarios. Depth 5")
    runScenarioSet(depth5ScoreVsDiffScenarios)
    printFooter("Score difference vs. state score scenarios. Depth 5")

    printHeader("Safe score vs. state score scenarios. Depth 4")
    runScenarioSet(depth4ScoreSafeScenarios)
    printFooter("Safe score vs. state score scenarios. Depth 4")

    printHeader("Aggressive score vs. state score scenarios. Depth 4")
    runScenarioSet(depth4ScoreAggScenarios)
    printFooter("Aggressive score vs. state score scenarios. Depth 4")

    printHeader("Difference in aggressive score vs. state score scenarios. Depth 4")
    runScenarioSet(depth4ScoreDiffAggScenarios)
    printFooter("Difference in aggressive score vs. state score scenarios. Depth 4")
  }

  def runGame(board: Board, boardName: String)(p1Utility: UtilityFn, p1Cutoff: CutoffFn)(p1Agent: Agent)
    (p2Utility: UtilityFn, p2Cutoff: CutoffFn)(p2Agent: Agent) {
    println("=" * 100)
    println(
      "Running " + p1Agent.description + " as Player 1 against " + p2Agent.description + " as Player 2 on board " +
        boardName)
    println("Player 1 cutoff: " + p1Cutoff.description)
    println("Player 2 cutoff: " + p2Cutoff.description)
    println("Player 1 utility: " + p1Utility.description)
    println("Player 2 utility: " + p2Utility.description)
    var player1NodesExpanded = 0
    var player1Moves = 0
    var player1TimeTaken: Long = 0
    var player2NodesExpanded = 0
    var player2Moves = 0
    var player2TimeTaken: Long = 0
    var state = State(board, Player1)
    while (!terminalTest(state)) {
      state match {
        case State(_, Player1) =>
          val ((action, cnt), millis) = time(p1Agent.search(p1Cutoff.cutoff)(p1Utility.utility)(state))
          state = result(state, action)
          player1NodesExpanded += cnt
          player1Moves += 1
          player1TimeTaken += millis
        case State(_, Player2) =>
          val ((action, cnt), millis) = time(p2Agent.search(p2Cutoff.cutoff)(p2Utility.utility)(state))
          state = result(state, action)
          player2NodesExpanded += cnt
          player2Moves += 1
          player2TimeTaken += millis
      }
    }
    println(("=" * 24) + " Game Ended " + ("=" * 24))
    printf("%18s%15s%15s\n", "", "Player 1", "Player 2")
    printf("%18s%,15d%,15d\n", "Score", score(state)(Player1), score(state)(Player2))
    printf("%18s%,15d%,15d\n", "Total Nodes", player1NodesExpanded, player2NodesExpanded)
    printf("%18s%,15d%,15d\n", "Avg Nodes/Move", player1NodesExpanded / player1Moves, player2NodesExpanded / player2Moves)
    printf("%18s%,15d%,15d\n", "Total Time (ms)", player1TimeTaken, player2TimeTaken)
    printf("%18s%,15d%,15d\n", "Avg Time/Move (ms)", player1TimeTaken / player1Moves, player2TimeTaken / player2Moves)
    println("=" * 60)
    println("Final board state:")
    printBoard(state.board)
    println("=" * 100)
  }


  def depthCutoff(d: Int)(state: State, depth: Int): Boolean = depth > d || terminalTest(state)

  type Search = Cutoff => Utility => State => (Action, Int)

  case class Agent(search: Search, description: String)

  val minimaxAgent = Agent(Minimax.apply, "Minimax agent")
  val alphabetaAgent = Agent(AlphaBetaSearch.apply, "Alpha Beta agent")

  case class UtilityFn(utility: Utility, description: String)

  def scoreUtil(player: Player) = UtilityFn(score(_)(player), "Player's score in current state.")

  def diffUtil(player: Player) = UtilityFn(state => score(state)(player) - score(state)(nextPlayer(player)),
    "Difference between Player's score in current state and enemy Player's")

  def safeUtil(player: Player) = UtilityFn(SafeScore(player),
    "Score in current state. All vulnerable pieces have their score reduced by a factor of 1")

  def aggUtil(player: Player) = UtilityFn(AggressiveScore(player),
    "Score in current state. All empty spaces \'protected\' by player piece add to score")

  def diffAggUtil(player: Player) =
    UtilityFn(state => AggressiveScore(player)(state) - AggressiveScore(nextPlayer(player))(state),
      "Difference between Player's score in current state and enemy Player's. "
        + "All places \'protected\' by player piece add to score")

  case class CutoffFn(cutoff: Cutoff, description: String)

  val depth3Cutoff = CutoffFn(depthCutoff(2), "Depth > 2 or state is a terminal state")
  val depth4Cutoff = CutoffFn(depthCutoff(3), "Depth > 3 or state is a terminal state")
  val depth5Cutoff = CutoffFn(depthCutoff(4), "Depth > 4 or state is a terminal state")
  val depth6Cutoff = CutoffFn(depthCutoff(5), "Depth > 5 or state is a terminal state")

  case class Scenario(p1Utility: UtilityFn, p1Cutoff: CutoffFn, p1Agent: Agent, p2Utility: UtilityFn, p2Cutoff: CutoffFn, p2Agent: Agent)

  val reqScenario1 = Scenario(scoreUtil(Player1), depth3Cutoff, minimaxAgent, scoreUtil(Player2), depth3Cutoff, minimaxAgent)
  val reqScenario2 = Scenario(scoreUtil(Player1), depth3Cutoff, alphabetaAgent, scoreUtil(Player2), depth3Cutoff, alphabetaAgent)
  val reqScenario3 = Scenario(scoreUtil(Player1), depth4Cutoff, minimaxAgent, scoreUtil(Player2), depth4Cutoff, minimaxAgent)
  val reqScenario4 = Scenario(scoreUtil(Player1), depth4Cutoff, alphabetaAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val reqScenario5 = Scenario(scoreUtil(Player1), depth3Cutoff, minimaxAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val reqScenario6 = Scenario(scoreUtil(Player1), depth3Cutoff, minimaxAgent, scoreUtil(Player2), depth5Cutoff, alphabetaAgent)
  val reqScenario7 = Scenario(scoreUtil(Player1), depth5Cutoff, alphabetaAgent, scoreUtil(Player2), depth5Cutoff, alphabetaAgent)
  val reqScenario8 = Scenario(scoreUtil(Player1), depth6Cutoff, alphabetaAgent, scoreUtil(Player2), depth6Cutoff, alphabetaAgent)
  val reqScenarios = List(reqScenario1, reqScenario2, reqScenario3, reqScenario4, reqScenario5, reqScenario6, reqScenario7, reqScenario8)

  val scenario21 = Scenario(diffUtil(Player1), depth4Cutoff, alphabetaAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val scenario22 = Scenario(scoreUtil(Player1), depth4Cutoff, alphabetaAgent, diffUtil(Player2), depth4Cutoff, alphabetaAgent)
  val depth4ScoreVsDiffScenarios = List(scenario21, scenario22)

  val scenario32 = Scenario(diffUtil(Player1), depth5Cutoff, alphabetaAgent, scoreUtil(Player2), depth5Cutoff, alphabetaAgent)
  val scenario33 = Scenario(scoreUtil(Player1), depth5Cutoff, alphabetaAgent, diffUtil(Player2), depth5Cutoff, alphabetaAgent)
  val depth5ScoreVsDiffScenarios = List(scenario32, scenario33)

  val scenario51 = Scenario(safeUtil(Player1), depth4Cutoff, alphabetaAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val scenario52 = Scenario(scoreUtil(Player1), depth4Cutoff, alphabetaAgent, safeUtil(Player2), depth4Cutoff, alphabetaAgent)
  val depth4ScoreSafeScenarios = List(scenario51, scenario52)

  val scenario61 = Scenario(aggUtil(Player1), depth4Cutoff, alphabetaAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val scenario62 = Scenario(scoreUtil(Player1), depth4Cutoff, alphabetaAgent, aggUtil(Player2), depth4Cutoff, alphabetaAgent)
  val depth4ScoreAggScenarios = List(scenario61, scenario62)

  val scenario71 = Scenario(diffAggUtil(Player1), depth4Cutoff, alphabetaAgent, scoreUtil(Player2), depth4Cutoff, alphabetaAgent)
  val scenario72 = Scenario(scoreUtil(Player1), depth4Cutoff, alphabetaAgent, diffAggUtil(Player2), depth4Cutoff, alphabetaAgent)
  val depth4ScoreDiffAggScenarios = List(scenario71, scenario72)

  def printHeader(scenarioSet: String) {
    println()
    println("=" * 100)
    println()
    println("Running scenario set " + scenarioSet)
    println()
    println("=" * 100)
    println()
  }

  def printNewBoard(board: Board, boardName: String) {
    println("=" * 100)
    println("Running scenarios on board " + boardName)
    println("Empty board:")
    printBoard(board)
    println("=" * 100)
  }

  def printFooter(scenarioSet: String) {
    println()
    println("=" * 100)
    println()
    println("Finished running scenario set " + scenarioSet)
    println()
    println("=" * 100)
    println()
  }

  def time[A](a: => A): (A, Long) = {
    val now = System.nanoTime
    val result = a
    val millis = (System.nanoTime - now) / 1000000
    (result, millis)
  }
}
