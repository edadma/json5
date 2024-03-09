package io.github.edadma.json5

import scala.collection.immutable.ListMap

abstract class Value

case class DoubleValue(n: Double) extends Value
case class NumberValue(n: String) extends Value
case class BooleanValue(b: Boolean) extends Value
case class StringValue(s: String) extends Value
case class ArrayValue(a: Seq[Value]) extends Value
case class ObjectValue(a: ListMap[String, Value]) extends Value
case object NullValue extends Value
