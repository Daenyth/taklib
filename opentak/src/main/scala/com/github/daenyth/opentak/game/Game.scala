package com.github.daenyth.opentak.game

import com.github.daenyth.opentak.ID
import com.github.daenyth.opentak.accounts.User
import com.github.daenyth.opentak.game.Game.GameId

object Game {
  type GameId = ID[Game]
  type SeekId = ID[Seek]
}
case class Game(id: GameId)
case class Seek(id: GameId, user: User, settings: SeekSettings)
case class SeekSettings()
