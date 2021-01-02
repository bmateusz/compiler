package compiler

import compiler.Types.Type
import compiler.elements.Element
import compiler.elements.Parameters.Parameter

import scala.util.matching.UnanchoredRegex

object Tokens {

  def parse(line: String): Option[Token] =
    findSimpleToken(line)
      .orElse(findStringLiteral(line))
      .orElse(findRegexToken(line))

  def findSimpleToken(line: String): Option[Token] =
    SimpleTokens.simpleTokens.find(token => line.startsWith(token.value))

  // val stringRegex: UnanchoredRegex = """^"((?:\\.|[^\\"])*)"""".r.unanchored
  def findStringLiteral(line: String): Option[Token] =
    line
      .iterator
      .scanLeft(0) {
        case (0, '"') => 1
        case (0, _) => -1
        case (1, '\\') => 2
        case (1, '"') => -2
        case (1, _) => 1
        case (2, _) => 1
      }
      .zipWithIndex
      .find(_._1 < 0)
      .flatMap {
        case (-2, value: Int) =>
          Some(StringLiteral(line.slice(1, value - 1)))
        case _ =>
          None
      }

  val floatRegex: UnanchoredRegex = "^(\\d+)(\\.\\d+(?:[eE]-?\\d+)?)?".r.unanchored
  val literalRegex: UnanchoredRegex = "^([a-zA-Z](?:[0-9a-zA-Z_-]*[0-9a-zA-Z])?)".r.unanchored

  def findRegexToken(line: String): Option[Token] =
    line match {
      case floatRegex(a, null) => Some(Integer(a.toLong))
      case floatRegex(a, b) => Some(Floating(s"$a$b".toDouble))
      case literalRegex(literal) => Some(Identifier(literal))
      case _ => None
    }

  def splitBySeparator[T](list: List[T], sep: T): List[List[T]] =
    list.span(_ != sep) match {
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
    def value: String

    def length: Int = value.length
  }

  sealed trait ParsedToken extends EvaluatedToken

  sealed trait Token extends ParsedToken

  case class Indentation(override val length: Int) extends Token {
    override def value: String = "\n" + " " * length

    def right: Indentation = copy(length + 1)
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
    def value: String
    def precedence: Int
    def leftAssociative: Boolean

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
    override def value: String = op.value
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
    override def value: String = integer.toString
  }

  case class Floating(double: Double) extends ValueToken {
    override def value: String = double.toString
  }

  case class ParsedCall(identifier: Identifier, expression: Expression) extends ParsedToken {
    override def value: String = s"${identifier.value}(${expression.tokens})"
  }

  case object EndMarker extends ParsedToken {
    override val value: String = s"END"
  }

  case class EvaluationError(token: EvaluationErrorToken) extends EvaluatedToken {
    override def value: String = token.value
  }

  sealed trait EvaluationErrorToken {
    def value: String
  }

  case class UnaryOperatorError(operator: Operators, token: EvaluatedToken) extends EvaluationErrorToken {
    override def value: String = s"${operator.value} ${token.value}"
  }

  case class OperatorError(operator: Operators, a: EvaluatedToken, b: EvaluatedToken) extends EvaluationErrorToken {
    override def value: String = s"${a.value} ${operator.value} ${b.value}"
  }

  case class ParameterTypeError(expected: Type, got: Type) extends EvaluationErrorToken {
    override def value: String = s"expected: ${expected.name}, got: ${got.name}"
  }

  case class ParameterTypeMismatchError(expected: List[Parameter], got: List[EvaluatedToken]) extends EvaluationErrorToken {
    override def value: String = s"expected: ${expected}, got: ${got}"
  }

  case class DefinitionReturnTypeMismatch(expected: Type, got: Type) extends EvaluationErrorToken {
    override def value: String = s"expected: ${expected}, got: ${got}"
  }

  case object DivisionByZero extends EvaluationErrorToken {
    override val value: String = "0 / 0"
  }

  case class UnexpectedIdentifier(token: Token) extends EvaluationErrorToken {
    override def value: String = token.value
  }

  case class UnexpectedIdentifierAfterDot(element: Element) extends EvaluationErrorToken {
    override def value: String = element.name.value
  }

  case class UnexpectedEvaluation(acc: List[EvaluatedToken], token: EvaluatedToken) extends EvaluationErrorToken {
    override def value: String = s"${token.value}, ${acc.map(_.value)}"
  }

  case class TooManyEvaluatedToken(tokens: List[EvaluatedToken]) extends EvaluationErrorToken {
    override def value: String = tokens.map(_.value).mkString(", ")
  }

  case object Pass extends EvaluatedToken {
    override val value: String = "pass"
  }

  case class ClassInstance(cls: elements.Class, values: List[EvaluatedToken]) extends EvaluatedToken {
    override def value: String = s"instance ${cls.name.value}($values)"
  }

  case class CallDefinition(definition: elements.Definition, values: List[EvaluatedToken]) extends EvaluatedToken {
    override def value: String = s"call ${definition.name.value}($values)"
  }

}
