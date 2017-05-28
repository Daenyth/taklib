package com.github.daenyth.opentak.server

import cats.syntax.all._
import cats.instances.list._
import cats.{Eq, Monad}
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.game.Game.{GameId, SeekId}
import com.github.daenyth.opentak.game.{Game, Seek}
import com.github.daenyth.opentak.server.Room.RoomId

import scala.language.higherKinds

case class SessionId(value: Int) extends AnyVal
object Server {
  implicit def serverEq[M[_]]: Eq[Server[M]] =
    (x: Server[M], y: Server[M]) =>
      x.users == y.users &&
        x.games == y.games &&
        x.seeks == y.seeks &&
        x.rooms == y.rooms
  val globalRoom: RoomId = RoomId(0)
}
trait Server[M[_]] {
  implicit def monad: Monad[M]

  def users: M[Map[SessionId, User]]
  def games: M[Map[GameId, Game]]
  def seeks: M[Map[SeekId, Seek]]
  def rooms: M[Map[RoomId, Room]]

  protected def addUser(user: User): M[SessionId]
  protected def removeUser(user: SessionId): M[Unit]

  def connect(user: User): M[SessionId] =
    users.flatMap { users_ =>
      users_
        .collectFirst {
          case (existingSessionId, u) if u === user => existingSessionId.pure[M]
        }
        .getOrElse {
          for {
            newSessionId <- addUser(user)
            _ <- addUserToRoom(newSessionId, Server.globalRoom)
          } yield newSessionId
        }
    }

  def disconnect(user: SessionId): M[Unit] =
    for {
      _ <- removeUserFromAllRooms(user)
      _ <- forfeitAllGames(user)
      _ <- removeUser(user)
    } yield ()

  def forfeitAllGames(user: SessionId): M[Unit]

  def removeUserFromAllRooms(user: SessionId): M[Unit] = {
    val removes: M[List[M[Unit]]] = rooms.map { allRooms =>
      allRooms.keys.toList.map(room => removeUserFromRoom(user, room))
    }
    removes.flatMap(_.sequence_)
  }

  def addSeek(seeker: User): M[(SeekId, Server[M])]
  def removeSeek(seek: SeekId): M[Unit]

  def chatRooms: Map[RoomId, Room]
  def addUserToRoom(user: SessionId, room: RoomId): M[Unit]
  def removeUserFromRoom(user: SessionId, room: RoomId): M[Unit]
  def sendChat(room: RoomId, sender: User, msg: String): M[Unit]
}
