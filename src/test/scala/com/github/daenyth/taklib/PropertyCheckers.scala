package com.github.daenyth.taklib

import org.scalacheck.{Prop, Properties}
import org.scalatest.prop.Checkers

import scala.language.implicitConversions

trait PropertyCheckers extends Checkers {
  implicit def propertiesToProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2): _*)
}
