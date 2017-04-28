package com.github.daenyth.taklib

import scala.util.control.NoStackTrace
import scalaz.Monad


sealed trait GameAction
case class StartGameWithBoard(board: Board) extends GameAction
sealed trait TurnAction extends GameAction {
  def ptn: String = this match {
    case PlayFlat(at) => at.name
    case PlayStanding(at) => s"S${at.name}"
    case PlayCapstone(at) => s"C${at.name}"
    case Move(from, direction, count, drops) =>
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
  def unapply(p: PlayStone): Option[(BoardIndex, Player => Stone)] =
    Some((p.at, p.stone))
}
sealed trait PlayStone extends TurnAction {
  def at: BoardIndex
  def stone: Player => Stone
}
case class PlayFlat(at: BoardIndex) extends PlayStone {
  val stone = FlatStone.apply _
}
case class PlayStanding(at: BoardIndex) extends PlayStone {
  val stone = StandingStone.apply _
}
case class PlayCapstone(at: BoardIndex) extends PlayStone {
  val stone = Capstone.apply _
}
case class Move(from: BoardIndex,
                direction: MoveDirection,
                count: Option[Int],
                drops: Option[Vector[Int]])
  extends TurnAction {
  def finalPosition: BoardIndex = {
    val moveDistance = drops.map(_.length).getOrElse(1)
    direction match {
      case Left => from.copy(file = from.file - moveDistance)
      case Right => from.copy(file = from.file + moveDistance)
      case Up => from.copy(rank = from.rank + moveDistance)
      case Down => from.copy(rank = from.rank - moveDistance)
    }
  }
}


object MoveResult {
  implicit val moveResultInstance: Monad[MoveResult] = new Monad[MoveResult] {

    override def map[A, B](fa: MoveResult[A])(f: (A) => B): MoveResult[B] =
      fa.map(f)

    override def bind[A, B](fa: MoveResult[A])(f: (A) => MoveResult[B]): MoveResult[B] =
      fa.flatMap(f)

    override def point[A](a: => A): MoveResult[A] = OkMove(a)
  }
}
sealed trait MoveResult[+A] {
  def map[B](f: A => B): MoveResult[B] = this match {
    case OkMove(nextState) => OkMove(f(nextState))
    case o: GameOver => o
    case i: InvalidMove => i
  }

  def flatMap[B](f: A => MoveResult[B]): MoveResult[B] = this match {
    case OkMove(nextState) => f(nextState)
    case o: GameOver => o
    case i: InvalidMove => i
  }

  def noteInvalid(t: TurnAction): MoveResult[A] = noteInvalid(r => s"(${t.ptn}) $r")
  def noteInvalid(addNote: String => String): MoveResult[A] = this match {
    case InvalidMove(reason) => InvalidMove(addNote(reason))
    case other => other
  }
}
case class OkMove[A](nextState: A) extends MoveResult[A]
case class GameOver(result: GameEndResult, finalState: Game) extends MoveResult[Nothing]
case class InvalidMove(reason: String)
  extends Exception(reason)
    with MoveResult[Nothing]
    with NoStackTrace



sealed trait MoveDirection { def name: String }
case object Left extends MoveDirection { val name = "<" }
case object Right extends MoveDirection { val name = ">" }
case object Up extends MoveDirection { val name = "+" }
case object Down extends MoveDirection { val name = "-" }
