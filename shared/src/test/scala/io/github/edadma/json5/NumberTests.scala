package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NumberTests extends AnyFreeSpec with Matchers:

  "number" in {
    parseFromString(" 123 ") shouldBe NumberValue("123")
  }
