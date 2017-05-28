package com.github.daenyth.opentak.server

import cats.Monad
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.game.Game
import com.github.daenyth.opentak.game.Game.GameId

import scala.language.higherKinds


abstract class Server[M[_]: Monad] {
  def users: Vector[User]

  /** Game ID -> real time game */
  def games: Map[Int, Game]

  def connect(user: User): M[Server[M]]
  def disconnect(user: User): M[Server[M]]

  def addSeek(seeker: User): M[(GameId, Server[M])]
  def removeSeek(seek: GameId): M[Server[M]]
}
