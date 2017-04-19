package com.github.daenyth.opentak.protocol

import com.github.daenyth.taklib.{PlayStone, Player, Move => TLMove}

// See protocol description at https://github.com/chaitu236/TakServer
object Playtak {
  case class GameNumber(value: Int) extends AnyVal
  case class Username(value: String) extends AnyVal
  case class RoomName(value: String) extends AnyVal

  sealed trait Incoming

  object Incoming {

    case class Client(version: String) extends Incoming
    case class Register(username: Username, email: String) extends Incoming

    trait Login extends Incoming
    case class UserLogin(username: Username, password: String) extends Login
    case object GuestLogin extends Login
    case object Logout

    case class Seek(size: Int, time: Int, increment: Int, asPlayer: Option[Player])
        extends Incoming
    case class Accept(gameNumber: GameNumber) extends Incoming

    /** Commands available to players in a game */
    sealed trait GameCommand extends Incoming {
      def gameNumber: GameNumber
    }
    case class Place(gameNumber: GameNumber, playStone: PlayStone) extends GameCommand
    case class Move(gameNumber: GameNumber, move: TLMove) extends GameCommand
    case class OfferDraw(gameNumber: GameNumber) extends GameCommand
    case class RescindDrawOffer(gameNumber: GameNumber) extends GameCommand
    case class Resign(gameNumber: GameNumber) extends GameCommand
    case class Show(gameNumber: GameNumber) extends GameCommand
    case class RequestUndo(gameNumber: GameNumber) extends GameCommand
    case class RescindUndoRequest(gameNumber: GameNumber) extends GameCommand

    case object ListSeeks extends Incoming
    case object ListGames extends Incoming

    case class Subscribe(gameNumber: GameNumber) extends Incoming
    case class Unsubscribe(gameNumber: GameNumber) extends Incoming

    case class Shout(msg: String) extends Incoming
    case class JoinRoom(name: RoomName) extends Incoming
    case class ShoutRoom(name: RoomName, msg: String) extends Incoming
    case class LeaveRoom(name: RoomName) extends Incoming
    case class Tell(username: Username, msg: String) extends Incoming

    case object Ping
  }

  sealed trait Outgoing

  object Outgoing {
    case object Welcome extends Outgoing
    case object LoginOrRegisterNow extends Outgoing
    case class WelcomeUser(username: Username) extends Outgoing
    case class GameListAdd(gameNumber: GameNumber,
                           whitePlayerusername: Username,
                           blackPlayerusername: Username,
                           size: Int,
                           time: Int,
                           increment: Int,
                           halfMovesPlayed: Int,
                           playerToMove: Player)
        extends Outgoing
    case class GameListRemove(gameNumber: GameNumber,
                              whitePlayerusername: Username,
                              blackPlayerusername: Username,
                              size: Int,
                              time: Int,
                              increment: Int,
                              halfMovesPlayed: Int,
                              playerToMove: Player)
        extends Outgoing
    case class GameStart(gameNumber: GameNumber,
                         size: Int,
                         whitePlayerusername: Username,
                         blackPlayerusername: Username,
                         yourColor: Player)
        extends Outgoing
    case class Place(gameNumber: GameNumber, playStone: PlayStone) extends Outgoing
    case class Move(gameNumber: GameNumber, move: TLMove) extends Outgoing
    case class UpdateTime(gameNumber: GameNumber, whiteTime: String, blackTime: String)
        extends Outgoing // TODO check what type the time values are
    case class GameOver(result: String) extends Outgoing
    case class DrawOffered(gameNumber: GameNumber) extends Outgoing
    case class DrawOfferRescinded(gameNumber: GameNumber) extends Outgoing
    case class UndoRequested(gameNumber: GameNumber) extends Outgoing
    case class UndoRequestRescinded(gameNumber: GameNumber) extends Outgoing
    case class PerformUndo(gameNumber: GameNumber) extends Outgoing
    case class GameAbandoned(gameNumber: GameNumber) extends Outgoing
    case class SeekAdded(gameNumber: GameNumber,
                         username: Username,
                         size: Int,
                         time: String,
                         asPlayer: Option[Player])
        extends Outgoing
    case class SeekRemoved(gameNumber: GameNumber,
                           username: Username,
                           size: Int,
                           time: String,
                           asPlayer: Option[Player])
        extends Outgoing
    case class ObserveGame(gameNumber: GameNumber,
                           whitePlayerusername: Username,
                           blackPlayerusername: Username,
                           size: Int,
                           time: String,
                           halfMovesPlayed: Int,
                           playerToMove: Player)
        extends Outgoing
    case class Shout(username: Username, msg: String) extends Outgoing
    case class RoomJoined(name: RoomName) extends Outgoing
    case class RoomLeft(name: RoomName) extends Outgoing
    case class ShoutRoom(name: RoomName, username: Username, msg: String) extends Outgoing
    case class Tell(username: Username, msg: String) extends Outgoing
    case class Told(username: Username, msg: String) extends Outgoing
    case class ServerMessage(msg: String) extends Outgoing
    case class Error(msg: String) extends Outgoing
    case class OnlineUsers(count: Int) extends Outgoing
    case object NOK extends Outgoing
    case object OK extends Outgoing
  }

}
