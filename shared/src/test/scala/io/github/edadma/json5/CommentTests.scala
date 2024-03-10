package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CommentTests extends AnyFreeSpec with Matchers:

  "multi 1" in {
    parseFromString(" /* 456 */ 123 ") shouldBe NumberValue("123")
  }

  "line 1" in {
    parseFromString("""
                      |// asdf
                      |123 // qwer
                      |// zxcv
                      |""".stripMargin) shouldBe NumberValue("123")
  }

  "line 2" in {
    parseFromString("""
                      |// asdf
                      |[ // qwer
                      |  3 // ;lkj
                      |  // zxcv
                      |]
                      |// poiu
                      |""".stripMargin) shouldBe ArrayValue(Seq(NumberValue("3")))
  }
