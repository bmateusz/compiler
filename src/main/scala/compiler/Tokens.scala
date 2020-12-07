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
      case floatRegex(a, null, _) => Some(Integer(a.toLong))
      case floatRegex(a, b, _) => Some(Floating(s"$a$b".toDouble))
      case literalRegex(literal, _) => Some(Identifier(literal))
      case _ => None
    }

  def splitBySeparator[T](list: List[T], sep: T): List[List[T]] =
    list.span( _ != sep ) match {
      case (hd, _ :: tl) => hd :: splitBySeparator(tl, sep)
      case (hd, _) => List(hd)
    }

  implicit class TokenListExtension[T >: Token](tokens: List[T]) {
    def spanMatchingRightParenthesis(): (List[T], List[T]) =
      tokens
        .scanLeft(1) {
          case (acc, LeftParenthesis) => acc + 1
          case (acc, RightParenthesis) => acc - 1
          case (acc, _) => acc
        }
        .zipWithIndex
        .find(_._1 == 0) match {
          case Some((0, value: Int)) =>
            tokens.splitAt(value - 1)
          case _ =>
            (tokens, List.empty)
        }

    def splitByComma(): List[List[T]] =
      splitBySeparator(tokens, Comma)
  }

  sealed trait EvaluatedToken {
    val value: String

    def length: Int = value.length
  }

  sealed trait ParsedToken extends EvaluatedToken

  sealed trait Token extends ParsedToken

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

  case object LeftBracket extends Token {
    override val value: String = "["
  }

  case object RightBracket extends Token {
    override val value: String = "]"
  }

  case object LeftCurlyBrace extends Token {
    override val value: String = "{"
  }

  case object RightCurlyBrace extends Token {
    override val value: String = "}"
  }

  case object Comma extends Token {
    override val value: String = ","
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

  case object Dot extends Operators {
    override val value: String = "."
    override val precedence: Int = 4
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
    val `[`: Token = LeftBracket
    val `]`: Token = RightBracket
    val `{`: Token = LeftCurlyBrace
    val `}`: Token = RightCurlyBrace
    val `,`: Token = Comma
    val `.`: Token = Operator(Dot)
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
      `)`,
      `[`,
      `]`,
      `{`,
      `}`,
      `,`,
      `.`,
      `+`,
      `-`,
      `*`,
      `/`,
      `???`,
      `if`,
      `else`,
    )

  }

  case class Identifier(override val value: String) extends Token

  object Identifier {
    def notUniqueIdentifiers(values: List[Identifier]): List[String] =
      values
        .groupMapReduce(_.value)(_ => 1)(_ + _)
        .filter(_._2 > 1)
        .keys
        .toList
        .sorted
  }

  sealed trait ValueToken extends Token

  case class StringLiteral(override val value: String) extends ValueToken {
    override def length: Int = value.length + 2 // the ""
  }

  case class Integer(integer: Long) extends ValueToken {
    override val value: String = integer.toString
  }

  case class Floating(double: Double) extends ValueToken {
    override val value: String = double.toString
  }

  case class ParsedCall(identifier: Identifier, expression: Expression) extends ParsedToken {
    override val value: String = s"${identifier.value}(${expression.tokens})"
  }

  case class EvaluationError(token: EvaluationErrorToken) extends EvaluatedToken {
    override val value: String = token.value
  }

  sealed trait EvaluationErrorToken {
    val value: String
  }

  case object DivisionByZero extends EvaluationErrorToken {
    override val value: String = "0 / 0"
  }

  case class UnexpectedIdentifier(token: Token) extends EvaluationErrorToken {
    override val value: String = token.value
  }

  case class UnexpectedEvaluation(acc: List[EvaluatedToken], token: EvaluatedToken) extends EvaluationErrorToken {
    override val value: String = s"${token.value}, ${acc.map(_.value)}"
  }

  case class ClassInstance(identifier: Identifier, values: List[List[EvaluatedToken]]) extends EvaluatedToken {
    override val value: String = s"${identifier.value}($values)"
  }

}
