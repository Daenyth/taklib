package com.github.daenyth.opentak.game

import com.github.daenyth.opentak.ID
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.game.Game.GameId

sealed trait Game
object Game {
  type GameId = ID[Game]
}
case class LiveGame(id: GameId)
case class Seek(id: GameId, user: User, settings: SeekSettings)
case class SeekSettings()
