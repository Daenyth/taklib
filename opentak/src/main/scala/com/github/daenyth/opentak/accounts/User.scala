package com.github.daenyth.opentak.accounts

import com.github.daenyth.opentak.ID

case class Username(value: String) extends AnyVal {
  override def toString: String = value
}

sealed trait User
object User {
  case class Registered(id: ID[Registered], name: Username) extends User
  case class Guest(name: Username) extends User
}
