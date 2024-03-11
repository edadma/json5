package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StringTests extends AnyFreeSpec with Matchers:

  "string using single quotes" in {
    parseFromString(" 'asdf' ") shouldBe StringValue("asdf")
  }

  "multi-line string" in {
    parseFromString(""" "Look, Mom! \
                      |No \\n's!" """.stripMargin) shouldBe StringValue("Look, Mom! \nNo \\n's!")
  }

  "string using double quotes" in {
    parseFromString(""" "asdf" """) shouldBe StringValue("asdf")
  }

  "bad string using single quotes" in {
    a[RuntimeException] should be thrownBy { parseFromString(" 'asdf ") }
  }

  "string escapes" in {
    parseFromString(raw""" '\'\"\\\b\f\n\r\t\v\0\x5E\u2713' """) shouldBe StringValue(
      "\'\"\\\b\f\n\r\t\u000B\u0000^\u2713",
    )
  }

  "bad escape 1" in {
    a[RuntimeException] should be thrownBy { parseFromString(raw" '\x5' ") }
  }

  "bad escape 2" in {
    a[RuntimeException] should be thrownBy { parseFromString(raw" '\u271' ") }
  }
