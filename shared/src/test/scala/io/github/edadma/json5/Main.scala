package io.github.edadma.json5

import pprint.pprintln

@main def run(): Unit =
  val v = parseFromString(
    raw""" '\b' """,
  )

  pprintln(v.asInstanceOf[StringValue].s.toList.map(c => c.toInt))
