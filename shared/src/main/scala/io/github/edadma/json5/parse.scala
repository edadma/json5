package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader
import io.github.edadma.char_reader.CharReader.EOI

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

def parseFromString(input: String): Value = parse(CharReader.fromString(input))

def parseFromFile(file: String): Value = parse(CharReader.fromFile(file))

def parse(r: CharReader): Value =
  def parseValue(r: CharReader): (CharReader, Value) =
    r.ch match
      case d if d.isDigit =>
        val (r1, n) = consumeWhile(r, c => c.isDigit || c == '.')

        (skipWhitespace(r1), NumberValue(n))
      case '[' => parseArray(r.next)
//      case '{' => parseObject(r.next)
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

//  def parseObject(r: CharReader): (CharReader, Value) =
//    val buf = new ListBuffer[(String, Value)]
//
//    def parseIdentifier(r: CharReader): (CharReader, String) =
//
//    def parseProperty(r: CharReader): Unit =
//
//    @tailrec
//    def parseObject(r: CharReader): (CharReader, Value) =
//      val r1 = skipWhitespace(r)
//
//      r1.ch match
//        case ',' if buf.nonEmpty =>
//          val (r2, v) = parseValue(r1.next)
//
//          buf += v
//          parseArray(r2)
//        case '}' => (r1.next, ArrayValue(buf.toList))
//        case _ if buf.isEmpty =>
//          val (r2, v) = parseValue(r1)
//
//          buf += v
//          parseArray(r2)
//        case _ => r1.error("expected ',' or '}'")
//
//    parseObject(r)

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
