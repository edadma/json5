package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

def parseFromString(input: String): Value = parse(CharReader.fromString(input))

def parseFromFile(file: String): Value = parse(CharReader.fromFile(file))

private val identifierRegex = "[_a-zA-Z][_a-zA-Z0-9]*".r

def parse(r: CharReader): Value =
  def parseValue(r: CharReader): (CharReader, Value) =
    r.ch match
      case d if d.isDigit =>
        val (r1, n) = consumeWhile(r, c => c.isDigit || c == '.')

        (skipWhitespace(r1), NumberValue(n))
      case '[' => parseArray(skipWhitespace(r.next))
//      case '{' => parseObject(skipWhitespace(r.next))
      case 'n' | 't' | 'f' =>
        val (r1, s) = consumeWhile(r, _.isLetter)
        val r2 = skipWhitespace(r1)

        s match
          case "null"  => (r2, NullValue)
          case "true"  => (r2, BooleanValue(true))
          case "false" => (r2, BooleanValue(false))
          case _       => r.error("unknown literal")
      case _ => r.error("a value was expected")

  def parseArray(r: CharReader): (CharReader, Value) =
    val buf = new ListBuffer[Value]

    @tailrec
    def parseArray(r: CharReader): (CharReader, Value) =
      r.ch match
        case ',' if buf.nonEmpty =>
          val (r1, v) = parseValue(skipWhitespace(r.next))

          buf += v
          parseArray(r1)
        case ']' => (skipWhitespace(r.next), ArrayValue(buf.toList))
        case _ if buf.isEmpty =>
          val (r1, v) = parseValue(r)

          buf += v
          parseArray(r1)
        case _ => r.error("expected ',' or ']'")

    parseArray(r)

  def parseObject(r: CharReader): (CharReader, Value) =
    val buf = new ListBuffer[(String, Value)]

    def parseIdentifier(r: CharReader): (CharReader, String) =
      val (r1, s) = consumeWhile(r, c => c.isLetterOrDigit || c == '_')

      if identifierRegex matches s then (skipWhitespace(r1), s)
      else r.error("valid identifier expected")

    def parseProperty(r: CharReader): CharReader =
      val (r1, s) = parseIdentifier(r)

      if r1.ch != ':' then r1.error("':' expected")

      val (r2, v) = parseValue(skipWhitespace(r1.next))

      buf += (s -> v)
      skipWhitespace(r2)

    @tailrec
    def parseObject(r: CharReader): (CharReader, ObjectValue) =
      r.ch match
        case ',' if buf.nonEmpty => parseObject(parseProperty(skipWhitespace(r.next)))
        case '}'                 => (skipWhitespace(r.next), ObjectValue(ListMap from buf.toList))
        case _ if buf.isEmpty    => parseObject(parseProperty(skipWhitespace(r)))
        case _                   => r.error("expected ',' or '}'")

    parseObject(r)

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

  val (r1, value) = parseValue(skipWhitespace(r))

  if r1.eoi then value
  else r1.error("end of input expected")
