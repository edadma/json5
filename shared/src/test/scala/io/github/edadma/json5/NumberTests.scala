package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class NumberTests extends AnyFreeSpec with Matchers:

  "integer" in {
    parseFromString(" 123 ") shouldBe NumberValue("123")
  }

  "integer + exponent" in {
    parseFromString(" 123e+02 ") shouldBe NumberValue("123e+02")
  }

  "positive number" in {
    parseFromString(" +123 ") shouldBe NumberValue("123")
  }

  "negative number" in {
    parseFromString(" -123 ") shouldBe NumberValue("-123")
  }

  "leading decimal" in {
    parseFromString(" .123 ") shouldBe NumberValue(".123")
  }

  "leading decimal + exponent" in {
    parseFromString(" .123e+02 ") shouldBe NumberValue(".123e+02")
  }

  "trailing decimal" in {
    parseFromString(" 123. ") shouldBe NumberValue("123.")
  }

  "trailing decimal + exponent" in {
    parseFromString(" 123.e+02 ") shouldBe NumberValue("123.e+02")
  }

  "decimal" in {
    parseFromString(" 0.123 ") shouldBe NumberValue("0.123")
  }

  "decimal + exponent" in {
    parseFromString(" 0.123e+02 ") shouldBe NumberValue("0.123e+02")
  }

  "hex" in {
    parseFromString(" 0x123 ") shouldBe NumberValue("0x123")
  }

  "bad number 1" in {
    a[RuntimeException] should be thrownBy { parseFromString(".") }
  }

  "bad number 2" in {
    a[RuntimeException] should be thrownBy { parseFromString("01") }
  }

  "bad number 3" in {
    a[RuntimeException] should be thrownBy { parseFromString("01.") }
  }

  "bad number 4" in {
    a[RuntimeException] should be thrownBy { parseFromString("0.1.") }
  }

  "bad number 5" in {
    a[RuntimeException] should be thrownBy { parseFromString("0..1") }
  }
