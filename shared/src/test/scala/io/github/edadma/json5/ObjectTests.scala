package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

class OjectTests extends AnyFreeSpec with Matchers:

  "empty object" in {
    parseFromString(" { } ") shouldBe ObjectValue(ListMap())
  }

  "one property object" in {
    parseFromString(" { a : 3 } ") shouldBe ObjectValue(ListMap("a" -> NumberValue("3")))
  }

  "two property object" in {
    parseFromString(" { a : 3 , b : 4 } ") shouldBe ObjectValue(
      ListMap("a" -> NumberValue("3"), "b" -> NumberValue("4")),
    )
  }

  "three property object" in {
    parseFromString(" { a : 3 , b : 4 , c : 5 } ") shouldBe ObjectValue(
      ListMap("a" -> NumberValue("3"), "b" -> NumberValue("4"), "c" -> NumberValue("5")),
    )
  }

  "object within object" in {
    parseFromString(" { a : 3 , b : { c : 4 , d : 5 } } ") shouldBe ObjectValue(
      ListMap("a" -> NumberValue("3"), "b" -> ObjectValue(ListMap("c" -> NumberValue("4"), "d" -> NumberValue("5")))),
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
      parseFromString("[3,]")
    }
  }

  "bad array 4" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("[,4]")
    }
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
