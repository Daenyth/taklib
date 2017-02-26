package com.github.daenyth.taklib

sealed trait GameAction
case class StartGameWithBoard(board: Board) extends GameAction
sealed trait TurnAction extends GameAction {
  def player: Player // TODO maybe get rid of this - makes ptn parsing hard

  def ptn: String = this match {
    case PlayFlat(_, at) => at.name
    case PlayStanding(_, at) => s"S${at.name}"
    case PlayCapstone(_, at) => s"C${at.name}"
    case Move(_, from, direction, count, drops) =>
      // Omit count+drops if moving whole stack or one piece
      val num =
        drops match {
          case Some(ds) if ds.length > 1 => count.map(_.toString).getOrElse("")
          case _ => ""
        }
      val dropSequence =
        drops match {
          case Some(ds) if ds.length > 1 => ds.mkString("")
          case _ => ""
        }
      s"$num${from.name}${direction.name}$dropSequence"
  }
}
object PlayStone {
  def unapply(p: PlayStone): Option[(BoardIndex, Stone)] =
    Some((p.at, p.stone))
}
sealed trait PlayStone extends TurnAction {
  def at: BoardIndex
  def stone: Stone
}
case class PlayFlat(player: Player, at: BoardIndex) extends PlayStone {
  val stone = FlatStone(player)
}
case class PlayStanding(player: Player, at: BoardIndex) extends PlayStone {
  val stone = StandingStone(player)
}
case class PlayCapstone(player: Player, at: BoardIndex) extends PlayStone {
  val stone = Capstone(player)
}
case class Move(player: Player,
                from: BoardIndex,
                direction: MoveDirection,
                count: Option[Int],
                drops: Option[Vector[Int]])
  extends TurnAction {
  def finalPosition: BoardIndex = {
    val moveDistance = drops.map(_.length).getOrElse(1)
    direction match {
      case Left => from.copy(rank = from.rank - moveDistance)
      case Right => from.copy(rank = from.rank + moveDistance)
      case Up => from.copy(file = from.file + moveDistance)
      case Down => from.copy(file = from.file - moveDistance)
    }
  }
}



sealed trait MoveDirection { def name: String }
case object Left extends MoveDirection { val name = "<" }
case object Right extends MoveDirection { val name = ">" }
case object Up extends MoveDirection { val name = "+" }
case object Down extends MoveDirection { val name = "-" }

case class InvalidMove(reason: String)
