package com.github.daenyth.opentak.protocol

import com.github.daenyth.taklib._

object PlaytakCodec {

  object Incoming {
    def decode(incoming: String): Either[String, Playtak.Incoming] = ???
  }

  object Outgoing {
    def encode(outgoing: Playtak.Outgoing): String = {
      import Playtak.Outgoing._
      outgoing match {
        case Welcome => "Welcome!"
        case LoginOrRegisterNow => "Login or Register"
        case WelcomeUser(username) => s"Welcome $username"
        case ge: GameEvent => encodeGameEvent(ge)
        case Shout(username, msg) => s"Shout <$username> $msg"
        case RoomJoined(name) => s"Joined room $name"
        case RoomLeft(name) => s"Left room $name"
        case ShoutRoom(name, username, msg) => s"ShoutRoom $name <$username> $msg"
        case Tell(username, msg) => s"Tell <$username> $msg"
        case Told(username, msg) => s"Told <$username> $msg"
        case ServerMessage(msg) => s"Message $msg"
        case Error(msg) => s"Error $msg"
        case OnlineUsers(count) => s"Online $count"
        case NOK => "NOK"
        case OK => "OK"
      }
    }

    private def encodeGameEvent(ge: Playtak.Outgoing.GameEvent): String = {
      import Playtak.Outgoing._
      ge match {
        case a: GameListAdd =>
          import a._
          val nextPlayer = playerToMove.fold(blackPlayerusername, whitePlayerusername)
          s"GameList Add Game#$gameNumber $whitePlayerusername vs $blackPlayerusername," +
            s" ${size}x${size}, $time, $increment, $halfMovesPlayed half-moves played, $nextPlayer to move"
        case r: GameListRemove =>
          import r._
          val nextPlayer = playerToMove.fold(blackPlayerusername, whitePlayerusername)
          s"GameList Remove Game#$gameNumber $whitePlayerusername vs $blackPlayerusername," +
            s" ${size}x${size}, $time, $increment, $halfMovesPlayed half-moves played, $nextPlayer to move"
        case s: GameStart =>
          import s._
          s"Game Start $gameNumber $size $whitePlayerusername vs $blackPlayerusername $yourColor"
        case Place(gameNumber, playStone) =>
          val stoneType = playStone.stone match {
            case _: Capstone => "C"
            case _: StandingStone => "W"
            case _: FlatStone => ""
          }
          s"Game#$gameNumber P ${playStone.at.name} $stoneType"
        case m: Move =>
          val start = m.start.name
          val end = m.end.name
          val drops = m.drops.mkString(" ")
          s"Game#${m.gameNumber} M $start $end $drops"
        case UpdateTime(gameNumber, whiteTime, blackTime) =>
          s"Game#$gameNumber Time $whiteTime $blackTime"
        case o: GameOver =>
          val result = o.result match {
            case RoadWin(player) => player.fold("0-R", "R-0")
            case DoubleRoad => "R-R" // Not actually supported by playtak or default rules, but different result sets can treat it differently.
            case FlatWin(player) => player.fold("0-F", "F-0")
            case Draw => "1/2-1/2"
            case WinByResignation(player) => player.fold("0-1", "1-0") // Again not supported by playtak; this is PTN format
          }
          s"Game#${o.gameNumber} Over $result"
        case DrawOffered(gameNumber) =>
          s"Game#$gameNumber OfferDraw"
        case DrawOfferRescinded(gameNumber) =>
          s"Game#$gameNumber RemoveDraw"
        case UndoRequested(gameNumber) =>
          s"Game#$gameNumber RequestUndo"
        case UndoRequestRescinded(gameNumber) =>
          s"Game#$gameNumber RemoveUndo"
        case PerformUndo(gameNumber) =>
          s"Game#$gameNumber Undo"
        case GameAbandoned(gameNumber) =>
          s"Game#$gameNumber Abandoned"
        case a: SeekAdded =>
          import a._
          val player = asPlayer.map(_.fold("B", "W")).getOrElse("")
          s"Seek new $gameNumber $username $size $time $player"
        case r: SeekRemoved =>
          import r._
          val player = asPlayer.map(_.fold("B", "W")).getOrElse("")
          s"Seek remove $gameNumber $username $size $time $player"
        case o: ObserveGame =>
          import o._
          val nextPlayer = playerToMove.fold(blackPlayerusername, whitePlayerusername)
          s"Observe Game#$gameNumber $whitePlayerusername vs $blackPlayerusername, ${size}x${size}," +
            s" $time, $halfMovesPlayed half-moves played, $nextPlayer to move"
      }
    }
  }
}
