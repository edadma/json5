package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader

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
      case '{' => parseObject(skipWhitespace(r.next))
      case 'n' | 't' | 'f' =>
        val (r1, s) = consumeWhile(r, _.isLetter)
        val r2 = skipWhitespace(r1)

        s match
          case "null"  => (r2, NullValue)
          case "true"  => (r2, BooleanValue(true))
          case "false" => (r2, BooleanValue(false))
          case _       => r.error("unknown literal")
      case '\'' | '"' => parseString(r)
      case _          => r.error("a value was expected")

  def parseString(r: CharReader): (CharReader, StringValue) =
    val delim = r.ch
    val (r1, s) = consumeWhile(r.next, _ != delim)

    if r1.eoi then r1.error("unclosed string")
    else (skipWhitespace(r1.next), StringValue(s))

  def parseArray(r: CharReader): (CharReader, ArrayValue) =
    val buf = new ListBuffer[Value]
    var comma: Boolean = false

    @tailrec
    def parseArray(r: CharReader): (CharReader, ArrayValue) =
      r.ch match
        case ']' => (skipWhitespace(r.next), ArrayValue(buf.toList))
        case ',' if comma =>
          comma = false
          parseArray(skipWhitespace(r.next))
        case _ if !comma =>
          val (r1, v) = parseValue(r)

          buf += v
          comma = true
          parseArray(r1)
        case _ => r.error("expected ',' or ']'")

    parseArray(r)

  def parseObject(r: CharReader): (CharReader, ObjectValue) =
    val buf = new ListBuffer[(String, Value)]
    var comma: Boolean = false

    def parseIdentifier(r: CharReader): (CharReader, String) =
      val (r1, s) = consumeWhile(r, c => c.isLetterOrDigit || c == '_')

      if identifierRegex matches s then (skipWhitespace(r1), s)
      else r.error("a valid identifier or string was expected")

    def parseProperty(r: CharReader): CharReader =
      val (r1, s) =
        if r.ch == '\'' || r.ch == '"' then
          val (r1, StringValue(s)) = parseString(r)

          (r1, s)
        else parseIdentifier(r)

      if r1.ch != ':' then r1.error("a ':' was expected")

      val (r2, v) = parseValue(skipWhitespace(r1.next))

      buf += (s -> v)
      r2

    @tailrec
    def parseObject(r: CharReader): (CharReader, ObjectValue) =
      r.ch match
        case ',' if comma =>
          comma = false
          parseObject(skipWhitespace(r.next))
        case '}' => (skipWhitespace(r.next), ObjectValue(ListMap from buf.toList))
        case _ if !comma =>
          comma = true
          parseObject(parseProperty(skipWhitespace(r)))
        case _ => r.error("expected ',' or '}'")

    parseObject(r)

  @tailrec
  def skipWhile(r: CharReader, pred: Char => Boolean): CharReader =
    if pred(r.ch) then skipWhile(r.next, pred)
    else r

  @tailrec
  def skipWhitespace(r: CharReader): CharReader =
    if r.ch.isWhitespace then skipWhitespace(r.next)
    else if r.ch == '/' && r.next.ch == '/' then skipWhitespace(skipWhile(r.next.next, _ != '\n'))
    else if r.ch == '/' && r.next.ch == '*' then
      @tailrec
      def skipComment(r: CharReader): CharReader =
        if r.ch == '*' && r.next.ch == '/' then skipWhitespace(r.next.next)
        else if r.eoi then r.error("unclosed comment")
        else skipComment(r.next)

      skipWhitespace(skipComment(r.next.next))
    else r

  def consumeWhile(r: CharReader, pred: Char => Boolean): (CharReader, String) =
    val buf = new StringBuilder

    @tailrec
    def consumeWhile(r: CharReader): (CharReader, String) =
      if pred(r.ch) && r.more then
        buf += r.ch

        consumeWhile(r.next)
      else (r, buf.toString)

    consumeWhile(r)

  val (r1, value) = parseValue(skipWhitespace(r))

  if r1.eoi then value
  else r1.error("end of input was expected")
