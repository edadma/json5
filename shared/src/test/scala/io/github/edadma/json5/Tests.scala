package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFreeSpec with Matchers {

  "number" in {
    parseValueFromString("123") shouldBe NumberValue("123")
  }

  "empty array" in {
    parseValueFromString("[]") shouldBe ArrayValue(Nil)
  }

  "one item array" in {
    parseValueFromString("[3]") shouldBe ArrayValue(Seq(NumberValue("3")))
  }

  "two item array" in {
    parseValueFromString("[3, 4]") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4")))
  }

  "three item array" in {
    parseValueFromString("[3, 4, 5]") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4"), NumberValue("5")))
  }

  "array within array" in {
    parseValueFromString("[3, [4, 5]]") shouldBe ArrayValue(
      Seq(NumberValue("3"), ArrayValue(Seq(NumberValue("4"), NumberValue("5")))),
    )
  }

  "null" in {
    parseValueFromString("null") shouldBe NullValue
  }

  "true" in {
    parseValueFromString("true") shouldBe BooleanValue(true)
  }

  "false" in {
    parseValueFromString("false") shouldBe BooleanValue(false)
  }

}
