package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StringTests extends AnyFreeSpec with Matchers:

  "string using single quotes" in {
    parseFromString(" 'asdf' ") shouldBe StringValue("asdf")
  }

  "string using double quotes" in {
    parseFromString(""" "asdf" """) shouldBe StringValue("asdf")
  }

  "bad string using single quotes" in {
    a[RuntimeException] should be thrownBy { parseFromString(" 'asdf ") }
  }
