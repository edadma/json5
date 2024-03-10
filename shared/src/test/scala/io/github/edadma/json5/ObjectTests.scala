package io.github.edadma.json5

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

class ObjectTests extends AnyFreeSpec with Matchers:

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

  "bad object 1" in {
    a[RuntimeException] should be thrownBy { parseFromString("{,}") }
  }

  "bad object 2" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("{a:3 4}")
    }
  }

  "bad object 3" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("{a:3,}")
    }
  }

  "bad object 4" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("{,b:4}")
    }
  }

  "bad object 5" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("{a}")
    }
  }

  "bad object 6" in {
    a[RuntimeException] should be thrownBy {
      parseFromString("{a:}")
    }
  }
