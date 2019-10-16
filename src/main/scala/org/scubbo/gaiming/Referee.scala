package org.scubbo.gaiming

import java.io._

class Referee() {

  def play(filePath: String,
           strategies: List[Strategy],
           gameState: GameState): List[Int] = {

    // Todo - check strategies for consistency? That the updates that they return are
    // of a type that gameState accepts?

    val numberOfPlayers = strategies.length

    val moveWriters: List[Writer] = (0 until numberOfPlayers).map(i =>
      new BufferedWriter(new FileWriter(new File(filePath + "/moves_" + i.toString)))).toList

    try {
      var playerIndex = 0
      // TODO - for most games, determination of completion does some of the "pre-calculation" of score,
      // so might be worthwhile to have it pass-back some state
      while (!gameState.isComplete()) {

        val strategy = strategies(playerIndex)
        val writer = moveWriters(playerIndex)

        try {

          val state = gameState
          val update = strategy.move(state)

          // Got to generate the serialization first, because the state's about to change!
          val repr = serialize(playerIndex, state, update)
          playerIndex = generateLegalMove(gameState, update, playerIndex)
          writer.write(repr + '\n')

        } catch {

          case e: IllegalMoveException =>
            // TODO - note down illegal move
            throw new RuntimeException("Not yet implemented", e)
          case e =>
            println(e)
            throw new RuntimeException("Unexpected exception", e)

        }
      }
      return gameState.getScore()
    } finally {
      moveWriters.foreach(w => w.close())
    }
  }

  // There's probably a more idiomatic way of doing this...but Scala doesn't have a break, so...
  // Damn, also, this doesn't make it easy to log the failed move. Blurp. Can do better.
  def generateLegalMove(gameState: GameState, update: Update, playerIndex: Int): Int = {
    for (i <- 1 to 3) {
      try {
        return gameState.apply(update, playerIndex)
      } catch {
        case e: IllegalMoveException =>
          //continue
        case e => throw e
      }
    }
    throw IllegalMoveException("Ran out of retries")
  }

  def serialize(idx: Int, state: GameState, update: Update): String = {
    return "{" + "\"strategy\":\"" + idx.toString + "\",\"state\":" + state.serialize() + ",\"update\":" + update.serialize() + "}"
  }

}
