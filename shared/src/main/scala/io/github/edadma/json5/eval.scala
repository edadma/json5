package io.github.edadma.json5

def eval(v: Value): Any =
  v match
    case NumberValue(n)      => n.toDouble
    case HexadecimalValue(h) => Integer.parseInt(h, 16)
