package com.github.daenyth.opentak.server

import com.github.daenyth.opentak.ID


case class RoomName(value: String) extends AnyVal {
  override def toString: String = value
}

case class Room(name: RoomName)
object Room {
  type RoomId = ID[Room]
}
