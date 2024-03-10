package io.github.edadma.json5

import io.github.edadma.char_reader.CharReader

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

def parseFromString(input: String): Value = parse(CharReader.fromString(input))

def parseFromFile(file: String): Value = parse(CharReader.fromFile(file))

private val identifierRegex = "[_$a-zA-Z][_$a-zA-Z0-9]*".r
private val numberRegex = raw"0x[0-9a-fA-F]+|(([1-9]\d*|0)(\.\d*)?|\.\d+)([eE][-+]?\d+)?".r

def parse(r: CharReader): Value =
  def parseValue(r: CharReader): (CharReader, Value) =
    r.ch match
      case '+' | '-' | '.' => parseNumeric(r)
      case c if c.isDigit  => parseNumeric(r)
      case '['             => parseArray(skipWhitespace(r.next))
      case '{'             => parseObject(skipWhitespace(r.next))
      case 'n' | 't' | 'f' | 'N' | 'I' =>
        val (r1, s) = consumeWhile(r, _.isLetter)
        val r2 = skipWhitespace(r1)

        s match
          case "null"     => (r2, NullValue)
          case "true"     => (r2, BooleanValue(true))
          case "false"    => (r2, BooleanValue(false))
          case "NaN"      => (r2, NaNValue)
          case "Infinity" => (r2, InfinityValue)
          case _          => r.error("unknown literal")
      case '\'' | '"' => parseString(r)
      case _          => r.error("a value was expected")

  def parseNumeric(r: CharReader): (CharReader, NumericValue) =
    val (r1, sign) =
      r.ch match
        case '-' => (r.next, "-")
        case '+' => (r.next, "+")
        case _   => (r, "")

    if sign.nonEmpty && (r1.ch == 'I' || r1.ch == 'N') then
      val (r2, s) = consumeWhile(r1, _.isLetter)
      val r3 = skipWhitespace(r2)

      s match
        case "NaN"      => (r3, NaNValue)
        case "Infinity" => (r3, if sign == "-" then NegativeInfinityValue else InfinityValue)
        case _          => r.error("unknown signed literal")
    else
      val (r2, n) =
        consumeWhile(r1, c => c.isDigit || ".xX-+aAbBcCdDeEfF".contains(c))

      if numberRegex matches n then
        val s = if sign == "-" then s"-$n" else n

        (skipWhitespace(r2), if s.toLowerCase.startsWith("0x") then HexadecimalValue(s drop 2) else NumberValue(s))
      else r.error("invalid numeric literal")

  def parseString(r: CharReader): (CharReader, StringValue) =
    val buf = new StringBuilder
    val delim = r.ch

    @tailrec
    def consume(r: CharReader): CharReader =
      r.ch match
        case `delim` => r.next
        case '\\' =>
          r.next.ch match
            case '\n' =>
              buf += '\n'
              consume(r.next.next)
            case '\'' =>
              buf += '\''
              consume(r.next.next)
            case '"' =>
              buf += '"'
              consume(r.next.next)
            case '\\' =>
              buf += '\\'
              consume(r.next.next)
            case 'b' =>
              buf += '\b'
              consume(r.next.next)
            case 'f' =>
              buf += '\f'
              consume(r.next.next)
            case 'n' =>
              buf += '\n'
              consume(r.next.next)
            case 'r' =>
              buf += '\r'
              consume(r.next.next)
            case 't' =>
              buf += '\t'
              consume(r.next.next)
            case 'v' =>
              buf += '\u000B'
              consume(r.next.next)
            case '0' =>
              buf += '\u0000'
              consume(r.next.next)
            case _ => r.next.error("unknown escape")
        case c =>
          buf += c
          consume(r.next)

    val r1 = consume(r.next)

    if r1.eoi then r1.error("unclosed string")
    else (skipWhitespace(r1), StringValue(buf.toString))

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
      val (r1, s) = consumeWhile(r, c => c.isLetterOrDigit || c == '_' || c == '$')

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
