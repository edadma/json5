package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CommentTests extends AnyFreeSpec with Matchers:

  "multi 1" in {
    parseFromString(" /* asdf */ 123 /* zxcv */  ") shouldBe NumberValue("123")
  }

  "multi 2" in {
    parseFromString("""
        |/**
        |  asdf
        |  zxcv
        |  */
        |123
        |""".stripMargin) shouldBe NumberValue("123")
  }

  "bad multi 1" in {
    a[RuntimeException] should be thrownBy { parseFromString("/* 456 ") }
  }

  "bad multi 2" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("/*")
    }
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
