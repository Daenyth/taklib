package com.github.daenyth.opentak.server

import cats.{Eq, Monad}
import com.github.daenyth.opentak.ID
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.game.{Game, Seek}
import com.github.daenyth.opentak.game.Game.{GameId, SeekId}
import com.github.daenyth.opentak.server.Room.RoomId

import scala.language.higherKinds

class Room
object Room {
  type RoomId = ID[Room]
}

object Server {
  implicit def serverEq[M[_]]: Eq[Server[M]] = new Eq[Server[M]] {
    override def eqv(x: Server[M], y: Server[M]): Boolean = ???
  }
}
abstract class Server[M[_]: Monad] {
  def users: Vector[User]

  def games: Map[GameId, Game]
  def seeks: Map[SeekId, Seek]

  def connect(user: User): M[Server[M]]
  def disconnect(user: User): M[Server[M]]

  def addSeek(seeker: User): M[(SeekId, Server[M])]
  def removeSeek(seek: SeekId): M[Server[M]]

  def chatRooms: Map[RoomId, Room]
  def addUserToRoom(user: User, room: RoomId): M[Server[M]]
  def removeUserFromRoom(user: User, room: RoomId): M[Server[M]]
  def sendChat(room: RoomId, sender: User, msg: String): M[Unit]
}
