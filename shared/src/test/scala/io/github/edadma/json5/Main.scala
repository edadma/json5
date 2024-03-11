package io.github.edadma.json5

import pprint.pprintln

@main def run(): Unit =
  val v = parseFromString(
    raw"""
         |{
         |  // comments
         |  unquoted: 'and you can quote me on that',
         |  singleQuotes: 'I can use "double quotes" here',
         |  lineBreaks: "Look, Mom! \
         |No \\n's!",
         |  hexadecimal: 0xdecaf,
         |  leadingDecimalPoint: .8675309, andTrailing: 8675309.,
         |  positiveSign: +1,
         |  trailingComma: 'in objects', andIn: ['arrays',],
         |  "backwardsCompatible": "with JSON",
         |}""".stripMargin,
  )

  pprintln(v)
  pprintln(eval(v))
