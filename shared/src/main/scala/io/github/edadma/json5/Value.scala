package io.github.edadma.json5

abstract class Value

abstract class NumericValue extends Value

case class NumberValue(n: String) extends NumericValue
case class HexadecimalValue(h: String) extends NumericValue
case class BooleanValue(b: Boolean) extends Value
case class StringValue(s: String) extends Value
case class ArrayValue(a: Seq[Value]) extends Value
case class ObjectValue(m: Map[String, Value]) extends Value
case object NullValue extends Value
case object InfinityValue extends NumericValue
case object NegativeInfinityValue extends NumericValue
case object NaNValue extends NumericValue
