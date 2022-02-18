package com.github.daenyth.taklib

import org.scalacheck.{Prop, Properties}
import org.scalatestplus.scalacheck.Checkers

trait PropertyCheckers extends Checkers {
  implicit def propertiesToProp(properties: Properties): Prop =
    Prop.all(properties.properties.map(_._2).toSeq: _*)
}
