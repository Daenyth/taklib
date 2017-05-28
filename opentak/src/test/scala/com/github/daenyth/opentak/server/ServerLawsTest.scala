package com.github.daenyth.opentak.server

import com.github.daenyth.opentak.accounts.User
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.language.higherKinds


object ServerLawsTest {
  type M[A] = Either[Throwable, A]
//  object ServerM extends Server[M] {}
//  val emptyServer: Gen[Server[M]] = Gen.const(ServerM)
}
class ServerLawsTest extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ServerLawsTest._

  def connectThenDisconnectId(s: Server[M], user: User): M[Boolean] =
    for {
    added <- s.connect(user)
    removed <- added.disconnect(user)
  } yield s === removed

  def addThenRemoveSeekId(s: Server[M], user: User): M[Boolean] =
    for {
      added <- s.addSeek(user)
      removed <- s.removeSeek(added._1)
    } yield s === removed


}
