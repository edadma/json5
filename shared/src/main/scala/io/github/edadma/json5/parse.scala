package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def parseValueFromString(input: String): Value = parseValue(CharReader.fromString(input))

def parseValueFromFile(file: String): Value = parseValue(CharReader.fromFile(file))

def parseValue(r: CharReader): Value =
  def parseValue(r: CharReader): (CharReader, Value) =
    val r1 = skipWhitespace(r)

    r1.ch match
      case d if d.isDigit =>
        val (r2, n) = consumeWhile(r1, c => c.isDigit || c == '.')

        (r2, NumberValue(n))
      case '[' => parseArray(r1.next)
      case _   => r.error("a value was expected")
  end parseValue

  def parseArray(r: CharReader): (CharReader, Value) =
    val buf = new ListBuffer[Value]

    @tailrec
    def parseArray(r: CharReader): (CharReader, Value) =
      val r1 = skipWhitespace(r)

      r1.ch match
        case ',' if buf.nonEmpty =>
          val (r2, v) = parseValue(r1.next)

          buf += v
          parseArray(r2)
        case ']' => (r1.next, ArrayValue(buf.toList))
        case _ =>
          val (r2, v) = parseValue(r1)

          buf += v
          parseArray(r2)

    parseArray(r)

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

  val (r1, value) = parseValue(r)

  if skipWhitespace(r1).eoi then value
  else r1.error("end of input expected")
