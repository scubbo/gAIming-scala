package org.scubbo.gaiming

import java.io.File

import scala.collection.mutable
import scala.util.Random

object Runner {
  // This is a scaffolding class!

  def main(args: Array[String]): Unit = {
    val rootDirectory = new File("data/games")


    for (_ <- 1 to 1000) {
      val gameIndex = rootDirectory.listFiles().length
      val gameDirectory = new File(rootDirectory.getPath + "/game_" + gameIndex.toString)
      gameDirectory.mkdir()


      val ref = new Referee()
      try {
        val output = ref.play(gameDirectory.getCanonicalPath, List(new TicTacStrategy, new TicTacStrategy), new TicTacGameState)
        return
      } catch {
        case e =>
      }
    }
    throw new RuntimeException("Failed :(")
  }

  class TicTacGameState extends GameState {

    val vals = mutable.MutableList(-1,-1,-1,
                                   -1,-1,-1,
                                   -1,-1,-1)

    override def apply(update: Update, playerNum: Int): Int = {
      if (!update.isInstanceOf[TicTacUpdate]) {
        throw IllegalMoveException("Not a TicTacUpdate")
      }

      val ticTacUpdate = update.asInstanceOf[TicTacUpdate]

      if (ticTacUpdate.row < 0 || ticTacUpdate.row > 2 || ticTacUpdate.column < 0 || ticTacUpdate.column > 2) {
        throw IllegalMoveException("Moves out of bounds: " + ticTacUpdate.row.toString + ":" + ticTacUpdate.column.toString)
      }

      if (playerNum < 0 || playerNum > 1) {
        throw IllegalMoveException("Only supports two players")
      }

      val index = 3*ticTacUpdate.row + ticTacUpdate.column
      if (vals(index) != -1) {
        throw IllegalMoveException("That space is already taken!")
      }

      println("Updating vals index " + index.toString + " to " + playerNum.toString)
      vals.update(index, playerNum)
      print(vals.mkString(","))
      return 1-playerNum

    }

    override def isComplete(): Boolean = {
      return rowComplete(0).isDefined || rowComplete(1).isDefined || rowComplete(2).isDefined||
        columnComplete(0).isDefined || columnComplete(1).isDefined || columnComplete(2).isDefined ||
        mainDiagComplete().isDefined || offDiagComplete.isDefined
    }

    private def rowComplete(i: Int): Option[Int] = {
      if (vals(3*i) == vals(3*i+1) && vals(3*i+1) == vals(3*i+2) && vals(3*i+2) != -1) {
        return Option.apply(vals(3*i))
      } else {
        return Option.empty
      }
    }

    private def columnComplete(i: Int): Option[Int] = {
      if (vals(i) == vals(3+i) && vals(3+i) == vals(6+i) && vals(6+i) != -1) {
        return Option.apply(vals(i))
      } else {
        return Option.empty
      }
    }

    private def mainDiagComplete(): Option[Int] = {
      if (vals(0) == vals(5) && vals(4) == vals(8) && vals(8) != -1) {
        return Option.apply(vals(0))
      } else {
        return Option.empty
      }

    }

    private def offDiagComplete(): Option[Int]= {
      if (vals(2) == vals(4) && vals(4) == vals(5) && vals(5) != -1) {
        return Option.apply(vals(2))
      } else {
        return Option.empty
      }
    }

    override def getScore(): List[Int] = {
      val winner = rowComplete(0).getOrElse(
        rowComplete(1).getOrElse(
          rowComplete(2).getOrElse(
            columnComplete(0).getOrElse(
              columnComplete(1).getOrElse(
                columnComplete(2).getOrElse(
                  mainDiagComplete().getOrElse(
                    offDiagComplete().get
                  )
                )
              )
            )
          )
        )
      )
      // MAN I'm sure there's a better way to do this :P
      return (0 to 1).map(i => if (i==winner) {1} else {0}).toList
    }

    override def serialize(): String = {
      return "{\"boardState\":[" + vals.mkString(",") + "]}"
    }
  }

  class TicTacUpdate(
                      val row: Int,
                      val column: Int) extends Update {
    override def serialize(): String = {
      return "{\"row\":" + row.toString + ",\"column\":" + column.toString + "}"
    }
  }

  class TicTacStrategy extends Strategy {

    val RANDOM: Random = new Random

    override def move(state: GameState): Update = {
      return new TicTacUpdate(RANDOM.nextInt(3), RANDOM.nextInt(3))
    }

  }

}
