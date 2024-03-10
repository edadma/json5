package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiteralTests extends AnyFreeSpec with Matchers:

  "null" in {
    parseFromString(" null ") shouldBe NullValue
  }

  "true" in {
    parseFromString(" true ") shouldBe BooleanValue(true)
  }

  "false" in {
    parseFromString(" false ") shouldBe BooleanValue(false)
  }

  "NaN" in {
    parseFromString(" NaN ") shouldBe NaNValue
  }

  "Infinity" in {
    parseFromString(" Infinity ") shouldBe InfinityValue
  }

  "+Infinity" in {
    parseFromString(" +Infinity ") shouldBe InfinityValue
  }

  "-Infinity" in {
    parseFromString(" -Infinity ") shouldBe NegativeInfinityValue
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
