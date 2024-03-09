package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

import scala.annotation.tailrec

def parseValueFromString(input: String): Value = parseValue(CharReader.fromString(input))

def parseValueFromFile(file: String): Value = parseValue(CharReader.fromFile(file))

def parseValue(r: CharReader): Value =
  @tailrec
  def skipWhitespace(r: CharReader): CharReader =
    if r.ch == ' ' || r.ch == '\n' || r.ch == '\t' then skipWhitespace(r.next)
    else r

  def consumeWhile(r: CharReader, pred: Char => Boolean): (CharReader, String) =
    val buf = new StringBuilder

    @tailrec
    def consumeWhile(r: CharReader): (CharReader, String) =
      if pred(r.ch) then
        buf += r.ch
        consumeWhile(r.next)
      else (r, buf.toString)

    consumeWhile(r)

  val r1 = skipWhitespace(r)

  r1.ch match
    case EOI => r.error("a value was expected")
    case d if d.isDigit =>
      val (r2, n) = consumeWhile(r1, c => c.isDigit || c == '.')

      NumberValue(n)
