package compiler

import scala.util.matching.UnanchoredRegex

object Tokens {

  def parse(line: String): Option[Token] =
    findSimpleToken(line)
      .orElse(findRegexToken(line))

  def findSimpleToken(line: String): Option[Token] =
    SimpleTokens.simpleTokens.find(token => line.startsWith(token.value))

  val stringRegex: UnanchoredRegex = """^"((\\.|[^\\"])*)"""".r.unanchored
  val floatRegex: UnanchoredRegex = "^(\\d+)(\\.\\d+([eE]-?\\d+)?)?".r.unanchored
  val literalRegex: UnanchoredRegex = "^([a-zA-Z]([0-9a-zA-Z_-]*[0-9a-zA-Z])?)".r.unanchored

  def findRegexToken(line: String): Option[Token] =
    line match {
      case stringRegex(str, _) => Some(StringLiteral(str))
      case floatRegex(a, null, _) => Some(Integer(a.toInt))
      case floatRegex(a, b, _) => Some(Floating(s"$a$b".toDouble))
      case literalRegex(literal, _) => Some(Identifier(literal))
      case _ => None
    }

  sealed trait EvaluatedToken {
    val value: String

    def length: Int = value.length
  }

  sealed trait Token extends EvaluatedToken

  case class Indentation(override val length: Int) extends Token {
    override val value: String = "\n" + " " * length
  }

  case object Def extends Token {
    override val value: String = "def"
  }

  case object Class extends Token {
    override val value: String = "class"
  }

  case object Enum extends Token {
    override val value: String = "enum"
  }

  case object NotImplemented extends Token {
    override val value: String = "???"
  }

  case object If extends Token {
    override val value: String = "if"
  }

  case object Else extends Token {
    override val value: String = "else"
  }

  case object Colon extends Token {
    override val value: String = ":"
  }

  case object Equals extends Token {
    override val value: String = "="
  }

  case object LeftParenthesis extends Token {
    override val value: String = "("
  }

  case object RightParenthesis extends Token {
    override val value: String = ")"
  }

  case object Comma extends Token {
    override val value: String = ","
  }

  case object Dot extends Token {
    override val value: String = "."
  }

  sealed trait Operators {
    val value: String
    val precedence: Int
    val leftAssociative: Boolean

    def hasGreaterPrecedenceThan(other: Operators): Boolean =
      precedence > other.precedence || (precedence == other.precedence && other.leftAssociative)
  }

  case object Add extends Operators {
    override val value: String = "+"
    override val precedence: Int = 2
    override val leftAssociative: Boolean = true
  }

  case object Subtract extends Operators {
    override val value: String = "-"
    override val precedence: Int = 2
    override val leftAssociative: Boolean = true
  }

  case object Multiply extends Operators {
    override val value: String = "*"
    override val precedence: Int = 3
    override val leftAssociative: Boolean = true
  }

  case object Divide extends Operators {
    override val value: String = "/"
    override val precedence: Int = 3
    override val leftAssociative: Boolean = true
  }

  case object Negate extends Operators {
    val value: String = "-"
    override val precedence: Int = 4
    override val leftAssociative: Boolean = false
  }

  case class Operator(op: Operators) extends Token {
    override val value: String = op.value
  }

  object SimpleTokens {
    val `def`: Token = Def
    val `class`: Token = Class
    val `enum`: Token = Enum
    val `???`: Token = NotImplemented
    val `if`: Token = If
    val `else`: Token = Else
    val `:`: Token = Colon
    val `=`: Token = Equals
    val `(`: Token = LeftParenthesis
    val `)`: Token = RightParenthesis
    val `,`: Token = Comma
    val `.`: Token = Dot
    val `+`: Token = Operator(Add)
    val `-`: Token = Operator(Subtract)
    val `*`: Token = Operator(Multiply)
    val `/`: Token = Operator(Divide)

    val simpleTokens = List(
      `def`,
      `:`,
      `=`,
      `class`,
      `enum`,
      `(`,
      `,`,
      `.`,
      `)`,
      `+`,
      `-`,
      `*`,
      `/`,
      `???`,
      `if`,
      `else`,
    )

    val simpleTokensMap: Map[String, Token] = simpleTokens.map(t => t.value -> t).toMap

  }

  case class Identifier(override val value: String) extends Token

  sealed trait ValueToken extends Token

  case class StringLiteral(override val value: String) extends ValueToken {
    override def length: Int = value.length + 2 // the ""
  }

  case class Integer(integer: Int) extends ValueToken {
    override val value: String = integer.toString
  }

  case class Floating(double: Double) extends ValueToken {
    override val value: String = double.toString
  }

  case object DivisionByZero extends EvaluatedToken {
    override val value: String = "0 / 0"
  }

}
