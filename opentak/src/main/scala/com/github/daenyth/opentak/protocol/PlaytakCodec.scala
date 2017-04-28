package com.github.daenyth.opentak.protocol

import com.github.daenyth.opentak.protocol.Playtak.{GameNumber, RoomName, Username}
import com.github.daenyth.taklib.Implicits.RichParsing
import com.github.daenyth.taklib._

import scala.util.parsing.combinator.RegexParsers

/**
 * Playtak protocol encoding/decoding between the wire
 * representation (string) and in-library representation (case classes)
 *
 * See PlaytakCodec#encode and PlaytakCodec#decode
 */
object PlaytakCodec {
  def encode(outgoing: Playtak.Outgoing): String =
    Outgoing.encode(outgoing)
  def decode(input: String): Either[String, Playtak.Incoming] =
    Incoming.parseEither(Incoming.incoming, input).toEither

  /* Abandon hope all ye who scroll below here */

  object Incoming extends RegexParsers with RichParsing {
    def decode(input: String): Either[String, Playtak.Incoming] =
      parseEither(incoming, input).toEither

    import Playtak.Incoming._

    val client: Parser[Client] = "Client" ~ "([A-Za-z-.0-9]{4,15})".r ^^ {
      case _ ~ s =>
        Client(s)
    }

    val msg: Parser[String] = """[^\n\r]{1,256}""".r
    val num: Parser[Int] = """\d""".r ^^ { _.toInt }
    val nums: Parser[Int] = """\d+""".r ^^ { _.toInt }
    val username: Parser[Username] = "[a-zA-Z][a-zA-Z0-9_]{3,15}".r ^^ Username.apply
    val email: Parser[String] = "[A-Za-z.0-9_+!#$%&'*^?=-]{1,30}@[A-Za-z.0-9-]{3,30}".r
    val password: Parser[String] = """[^\n\r\s]{6,50}""".r
    val gameNumber: Parser[GameNumber] = "Game#" ~ nums ^^ { case _ ~ n => GameNumber(n) }
    val boardIndex: Parser[BoardIndex] = "([ABCDEFGH])([12345678])".r ^^ { str =>
      val fileChr = str.charAt(0)
      val file = "_ABCDEFGH".toCharArray.indexOf(fileChr)
      val rank = str.charAt(1).toString.toInt
      BoardIndex(file, rank)
    }
    val roomName: Parser[RoomName] = """[^\n\r\\s]{4,15}""".r ^^ RoomName
    def simpleGameMessage[A](str: String, gameMsg: GameNumber => A): Parser[A] =
      gameNumber <~ str ^^ gameMsg

    val register: Parser[Register] = "Register" ~> username ~ email ^^ {
      case username ~ email =>
        Register(username, email)
    }
    val userLogin: Parser[UserLogin] = "Login" ~> username ~ password ^^ {
      case username ~ password =>
        UserLogin(username, password)
    }
    val guestLogin: Parser[GuestLogin.type] = "Login Guest" ^^^ GuestLogin
    val logout: Parser[Logout.type] = "^quit$".r ^^^ Logout
    val seek: Parser[Seek] = "Seek" ~> num ~ nums ~ nums ~ "[WB]?".r ^^ {
      case size ~ time ~ increment ~ color =>
        val asPlayer = color match {
          case "W" => Some(White)
          case "B" => Some(Black)
          case _ => None
        }
        Seek(size, time, increment, asPlayer)
    }
    val accept: Parser[Accept] = "Accept" ~> nums ^^ { n =>
      Accept(GameNumber(n))
    }
    val place: Parser[Place] = gameNumber ~ " P " ~ boardIndex ~ "[CW]?" ^^ {
      case gameNumber ~ _ ~ idx ~ stoneType =>
        val playStone = stoneType match {
          case "C" => PlayCapstone(idx)
          case "W" => PlayStanding(idx)
          case _ => PlayFlat(idx)
        }
        Place(gameNumber, playStone)
    }
    val move: Parser[Move] = gameNumber ~ " M " ~ boardIndex ~ boardIndex ~ rep(num) ^^ {
      case n ~ _ ~ start ~ end ~ drops => Move(n, start, end, drops.toVector)
    }
    val offerDraw: Parser[OfferDraw] = simpleGameMessage("OfferDraw", OfferDraw)
    val rescindDrawOffer: Parser[RescindDrawOffer] =
      simpleGameMessage("RemoveDraw", RescindDrawOffer)
    val resign: Parser[Resign] = simpleGameMessage("Resign", Resign)
    val show: Parser[Show] = simpleGameMessage("Show", Show)
    val requestUndo: Parser[RequestUndo] = simpleGameMessage("RequestUndo", RequestUndo)
    val rescindUndoRequest: Parser[RescindUndoRequest] =
      simpleGameMessage("RemoveUndo", RescindUndoRequest)
    val listSeeks: Parser[ListSeeks.type] = "^List$".r ^^^ ListSeeks
    val listGames: Parser[ListGames.type] = "^GameList$".r ^^^ ListGames
    val subscribe: Parser[Subscribe] = "Observe" ~> nums ^^ { n =>
      Subscribe(GameNumber(n))
    }
    val unsubscribe: Parser[Unsubscribe] = "Unobserve" ~> nums ^^ { n =>
      Unsubscribe(GameNumber(n))
    }
    val shout: Parser[Shout] = "Shout" ~> msg ^^ Shout
    val joinRoom: Parser[JoinRoom] = "JoinRoom" ~> roomName ^^ JoinRoom
    val leaveRoom: Parser[LeaveRoom] = "LeaveRoom" ~> roomName ^^ LeaveRoom
    val shoutRoom: Parser[ShoutRoom] = "ShoutRoom" ~> roomName ~ msg ^^ {
      case room ~ msg => ShoutRoom(room, msg)
    }
    val tell: Parser[Tell] = "Tell" ~> username ~ msg ^^ { case user ~ msg => Tell(user, msg) }
    val ping: Parser[Ping.type] = "^PING$".r ^^^ Ping

    val incoming: Parser[Playtak.Incoming] =
      (client | register | userLogin | guestLogin
        | logout | seek | accept | place | move | offerDraw | rescindDrawOffer
        | resign | show | requestUndo | rescindUndoRequest | listSeeks | listGames
        | subscribe | unsubscribe | shout | joinRoom | shoutRoom | leaveRoom | tell | ping)

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
          import Stone._
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
          import GameEndResult._
          val result = o.result match {
            case RoadWin(player) => player.fold("0-R", "R-0")
            case DoubleRoad =>
              "R-R" // Not actually supported by playtak or default rules, but different result sets can treat it differently.
            case FlatWin(player) => player.fold("0-F", "F-0")
            case Draw => "1/2-1/2"
            case WinByResignation(player) =>
              player.fold("0-1", "1-0") // Again not supported by playtak; this is PTN format
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
