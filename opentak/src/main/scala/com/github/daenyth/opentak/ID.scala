package com.github.daenyth.opentak

import cats.Eq

/** An ID number for some entity */
final case class ID[A](value: Int) extends AnyVal
object ID {
  implicit def idEq[A]: Eq[ID[A]] = (x: ID[A], y: ID[A]) => x.value == y.value
}
