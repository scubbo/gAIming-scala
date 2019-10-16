package org.scubbo.gaiming

trait Strategy {
  def move(state: GameState): Update
}
