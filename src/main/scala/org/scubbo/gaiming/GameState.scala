package org.scubbo.gaiming

// Should this be trait `GameState[UpdateType <: Update]` ?
trait GameState {

  // Returns the nextPlayerIndex
  @throws(classOf[IllegalMoveException])
  def apply(update: Update, playerNum: Int): Int

  def isComplete(): Boolean
  def getScore(): List[Int]
  def serialize(): String

}

trait Update {
  def serialize(): String
}
