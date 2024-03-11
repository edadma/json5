package io.github.edadma.json5

import scala.collection.immutable.ListMap

def eval(v: Value): Any =
  v match
    case NumberValue(n)        => n.toDouble
    case HexadecimalValue(h)   => Integer.parseInt(h, 16)
    case BooleanValue(b)       => b
    case StringValue(s)        => s
    case ArrayValue(a)         => a map eval
    case ObjectValue(m)        => (m.view mapValues eval) to ListMap
    case NullValue             => null
    case InfinityValue         => Double.PositiveInfinity
    case NegativeInfinityValue => Double.NegativeInfinity
    case NaNValue              => Double.NaN
