package io.github.edadma.json5

import pprint.pprintln

@main def run(): Unit =
  val v = parseValueFromString(
    """null4""",
  )

  pprintln(v)
