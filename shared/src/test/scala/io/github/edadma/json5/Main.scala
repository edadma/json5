package io.github.edadma.json5

import pprint.pprintln

@main def run(): Unit =
  val v = parseFromString(
    raw""" 0x123 """,
  )

  pprintln(v)
  pprintln(eval(v))
