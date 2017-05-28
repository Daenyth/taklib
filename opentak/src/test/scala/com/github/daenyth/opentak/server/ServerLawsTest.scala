package com.github.daenyth.opentak.server

import cats.Monad
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.functor._
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.server.Room.RoomId

import scala.language.higherKinds

object ServerLaws {
//  object ServerM extends Server[M] {}
//  val emptyServer: Gen[Server[M]] = Gen.const(ServerM)
}
abstract class ServerLaws[M[_]: Monad] {

  def testIdentity[A](f: Server[M] => M[A])(g: A => M[Server[M]]): Server[M] => M[Boolean] =
    s =>
      for {
        s1 <- f(s)
        s2 <- g(s1)
      } yield s === s2

  def testIdempotent(f: Server[M] => M[Server[M]]): Server[M] => M[Boolean] =
    s =>
      for {
        s1 <- f(s)
        s2 <- f(s1)
      } yield s1 === s2

  def connectThenDisconnectId(user: User) =
    testIdentity(_.connect(user))(_.disconnect(user))

  def addThenRemoveSeekId(user: User) =
    testIdentity(_.addSeek(user)) {
      case (seekId, server) => server.removeSeek(seekId)
    }

  def removeRoomIdempotent(room: RoomId, user: User) =
    testIdempotent(_.removeUserFromRoom(user, room))

  def addRoomIdempotent(s: Server[M], room: RoomId, user: User) =
    testIdempotent(_.addUserToRoom(user, room))

  def addedUserExists(user: User)(s: Server[M]) =
    for {
      s1 <- s.connect(user)
    } yield s1.users.exists { u: User => u === user }

  def removedUserIsGone(user: User)(s: Server[M]) =
    for {
      s1 <- s.disconnect(user)
    } yield !s1.users.exists { u: User => u === user }

}
