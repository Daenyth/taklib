package com.github.daenyth.opentak.accounts

import cats.Eq
import cats.syntax.eq._
import com.github.daenyth.opentak.ID

object Username {
  implicit val objectEq: Eq[Username] = (x: Username, y: Username) => x.value == y.value
}
case class Username(value: String) extends AnyVal {
  override def toString: String = value
}

sealed trait User
object User {
  implicit val userEq: Eq[User] = (x: User, y: User) => (x, y) match {
    case (x: Registered, y: Registered) => x.id === y.id
    case (x: Guest, y: Guest) => x.name === y.name
    case _ => false
  }
  case class Registered(id: ID[Registered], name: Username) extends User
  case class Guest(name: Username) extends User
}
