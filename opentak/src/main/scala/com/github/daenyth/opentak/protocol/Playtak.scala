package com.github.daenyth.opentak.protocol

import com.github.daenyth.taklib.{PlayStone, Player, Move => TLMove}

// See protocol description at https://github.com/chaitu236/TakServer
object Playtak {
  sealed trait Incoming

  object Incoming {

    case class Client(name: String) extends Incoming
    case class Register(username: String, email: String) extends Incoming

    trait Login extends Incoming
    case class UserLogin(username: String, password: String) extends Login
    case object GuestLogin extends Login
    case object Logout

    case class Seek(size: Int, time: Int, increment: Int, asPlayer: Option[Player])
        extends Incoming
    case class Accept(gameNumber: Int) extends Incoming

    /** Commands available to players in a game */
    sealed trait GameCommand extends Incoming {
      def gameNumber: Int
    }
    case class Place(gameNumber: Int, playStone: PlayStone) extends GameCommand
    case class Move(gameNumber: Int, move: TLMove) extends GameCommand
    case class OfferDraw(gameNumber: Int) extends GameCommand
    case class RescindDrawOffer(gameNumber: Int) extends GameCommand
    case class Resign(gameNumber: Int) extends GameCommand
    case class Show(gameNumber: Int) extends GameCommand
    case class RequestUndo(gameNumber: Int) extends GameCommand
    case class RescindUndoRequest(gameNumber: Int) extends GameCommand

    case object ListSeeks extends Incoming
    case object ListGames extends Incoming

    case class Subscribe(gameNumber: Int) extends Incoming
    case class Unsubscribe(gameNumber: Int) extends Incoming

    case class Shout(msg: String) extends Incoming
    case class JoinRoom(roomName: String) extends Incoming
    case class ShoutRoom(roomName: String, msg: String) extends Incoming
    case class LeaveRoom(roomName: String) extends Incoming
    case class Tell(username: String, msg: String) extends Incoming

    case object Ping
  }

  sealed trait Outgoing

  object Outgoing {
    case object Welcome extends Outgoing
    case object LoginOrRegisterNow extends Outgoing
    case class WelcomeUser(username: String) extends Outgoing
    case class GameListAdd(gameNumber: Int,
                           whitePlayerUsername: String,
                           blackPlayerUsername: String,
                           size: Int,
                           time: Int,
                           increment: Int,
                           halfMovesPlayed: Int,
                           playerToMove: Player)
        extends Outgoing
    case class GameListRemove(gameNumber: Int,
                              whitePlayerUsername: String,
                              blackPlayerUsername: String,
                              size: Int,
                              time: Int,
                              increment: Int,
                              halfMovesPlayed: Int,
                              playerToMove: Player)
        extends Outgoing
    case class GameStart(gameNumber: Int,
                         size: Int,
                         whitePlayerUsername: String,
                         blackPlayerUsername: String,
                         yourColor: Player)
        extends Outgoing
    case class Place(gameNumber: Int, playStone: PlayStone) extends Outgoing
    case class Move(gameNumber: Int, move: TLMove) extends Outgoing
    case class UpdateTime(gameNumber: Int, whiteTime: String, blackTime: String) extends Outgoing // TODO check what type the time values are
    case class GameOver(result: String) extends Outgoing
    case class DrawOffered(gameNumber: Int) extends Outgoing
    case class DrawOfferRescinded(gameNumber: Int) extends Outgoing
    case class UndoRequested(gameNumber: Int) extends Outgoing
    case class UndoRequestRescinded(gameNumber: Int) extends Outgoing
    case class PerformUndo(gameNumber: Int) extends Outgoing
    case class GameAbandoned(gameNumber: Int) extends Outgoing
    case class SeekAdded(gameNumber: Int,
                         username: String,
                         size: Int,
                         time: String,
                         asPlayer: Option[Player])
        extends Outgoing
    case class SeekRemoved(gameNumber: Int,
                           username: String,
                           size: Int,
                           time: String,
                           asPlayer: Option[Player])
        extends Outgoing
    case class ObserveGame(gameNumber: Int,
                           whitePlayerUsername: String,
                           blackPlayerUsername: String,
                           size: Int,
                           time: String,
                           halfMovesPlayed: Int,
                           playerToMove: Player)
        extends Outgoing
    case class Shout(username: String, msg: String) extends Outgoing
    case class RoomJoined(name: String) extends Outgoing
    case class RoomLeft(name: String) extends Outgoing
    case class ShoutRoom(name: String, username: String, msg: String) extends Outgoing
    case class Tell(username: String, msg: String) extends Outgoing
    case class Told(username: String, msg: String) extends Outgoing
    case class ServerMessage(msg: String) extends Outgoing
    case class Error(msg: String) extends Outgoing
    case class OnlineUsers(count: Int) extends Outgoing
    case object NOK extends Outgoing
    case object OK extends Outgoing
  }

}
