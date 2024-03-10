package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers:

  "number" in {
    parseFromString(" 123 ") shouldBe NumberValue("123")
  }

  "null" in {
    parseFromString(" null ") shouldBe NullValue
  }

  "true" in {
    parseFromString(" true ") shouldBe BooleanValue(true)
  }

  "false" in {
    parseFromString(" false ") shouldBe BooleanValue(false)
  }

  "bad literal 1" in {
    a[RuntimeException] should be thrownBy { parseFromString("asdf") }
  }

  "bad literal 2" in {
    a[RuntimeException] should be thrownBy { parseFromString("nullx") }
  }

  "bad literal 3" in {
    a[RuntimeException] should be thrownBy { parseFromString("null5") }
  }
