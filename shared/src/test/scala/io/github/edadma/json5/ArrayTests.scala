package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArrayTests extends AnyFreeSpec with Matchers:

  "empty array" in {
    parseFromString(" [ ] ") shouldBe ArrayValue(Nil)
  }

  "one item array" in {
    parseFromString(" [ 3 ] ") shouldBe ArrayValue(Seq(NumberValue("3")))
  }

  "one item array with trailing comma" in {
    parseFromString(" [ 3 , ] ") shouldBe ArrayValue(Seq(NumberValue("3")))
  }

  "two items array" in {
    parseFromString(" [ 3 , 4 ] ") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4")))
  }

  "two items array with trailing comma" in {
    parseFromString(" [ 3 , 4 , ] ") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4")))
  }

  "three items array" in {
    parseFromString(" [ 3 , 4 , 5 ] ") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4"), NumberValue("5")))
  }

  "three items array with trailing comma" in {
    parseFromString(" [ 3 , 4 , 5 , ] ") shouldBe ArrayValue(Seq(NumberValue("3"), NumberValue("4"), NumberValue("5")))
  }

  "array within array" in {
    parseFromString(" [ 3 , [ 4 , 5 ] ] ") shouldBe ArrayValue(
      Seq(NumberValue("3"), ArrayValue(Seq(NumberValue("4"), NumberValue("5")))),
    )
  }

  "bad array 1" in {
    a[RuntimeException] should be thrownBy { parseFromString("[,]") }
  }

  "bad array 2" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("[3 4]")
    }
  }

  "bad array 3" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("[,4]")
    }
  }
