package com.github.daenyth.opentak.server

import com.github.daenyth.opentak.ID


case class RoomName(value: String) extends AnyVal {
  override def toString: String = value
}

case class Room(name: RoomName, users: Set[SessionId]) {
  def removeUser(user: SessionId): Room = copy(users = users - user)
}
object Room {
  type RoomId = ID[Room]
  object RoomId {
    def apply(id: Int): RoomId = ID[Room](id)
  }
}
