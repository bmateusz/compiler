package compiler

import compiler.Types.Type
import compiler.elements.Element
import compiler.elements.Parameters.Parameter

import scala.util.chaining.scalaUtilChainingOps

object Tokens {

  def parse(line: String): Option[Token] =
    findSimpleToken(line)
      .orElse(findStringLiteral(line))
      .orElse(findNumberLiteral(line))
      .orElse(findLiteral(line))

  def findSimpleToken(line: String): Option[Token] =
    SimpleTokens.simpleTokens.find(token => line.startsWith(token.value))

  sealed trait StringParseMode

  case object ExpectQuote extends StringParseMode

  case object InString extends StringParseMode

  case object InEscape extends StringParseMode

  case object StringParseFailed extends StringParseMode

  case object StringParseSuccess extends StringParseMode

  def findStringLiteral(line: String): Option[Token] =
    line
      .iterator
      .scanLeft(ExpectQuote: StringParseMode) {
        case (ExpectQuote, '"') => InString
        case (ExpectQuote, _) => StringParseFailed
        case (InString, '\\') => InEscape
        case (InString, '"') => StringParseSuccess
        case (InString, _) => InString
        case (InEscape, _) => InString
        case (_, _) => StringParseFailed
      }
      .zipWithIndex
      .find {
        case (StringParseSuccess | StringParseFailed, _) => true
        case _ => false
      }
      .flatMap {
        case (StringParseSuccess, index: Int) =>
          Some(WideToken(StringLiteral(line.slice(1, index - 1)), index))
        case _ =>
          None
      }

  sealed trait NumberParseMode

  case object ExpectFirstDigit extends NumberParseMode

  case object ExpectDigitOrPoint extends NumberParseMode

  case object ExpectDigitAfterPoint extends NumberParseMode

  case object ExpectDigitOrEAfterPoint extends NumberParseMode

  case object ExpectDigitOrMinusAfterE extends NumberParseMode

  case object ExpectDigitAfterE extends NumberParseMode

  case object NumberParseFinished extends NumberParseMode

  def findNumberLiteral(line: String): Option[Token] =
    line
      .scanLeft(ExpectFirstDigit: NumberParseMode) {
        case (ExpectFirstDigit, char: Char) =>
          if (isDigit(char)) ExpectDigitOrPoint else NumberParseFinished
        case (ExpectDigitOrPoint, char: Char) =>
          if (isDigit(char)) ExpectDigitOrPoint else if (char == '.') ExpectDigitAfterPoint else NumberParseFinished
        case (ExpectDigitAfterPoint, char: Char) =>
          if (isDigit(char)) ExpectDigitOrEAfterPoint else NumberParseFinished
        case (ExpectDigitOrEAfterPoint, char: Char) =>
          if (isDigit(char)) ExpectDigitOrEAfterPoint else if (char == 'e' || char == 'E') ExpectDigitOrMinusAfterE else NumberParseFinished
        case (ExpectDigitOrMinusAfterE, char: Char) =>
          if (isDigitOrMinus(char)) ExpectDigitAfterE else NumberParseFinished
        case (ExpectDigitAfterE, char: Char) =>
          if (isDigit(char)) ExpectDigitAfterE else NumberParseFinished
        case _ =>
          NumberParseFinished
      }
      .takeWhile(_ != NumberParseFinished)
      .pipe { result =>
        if (result.isEmpty)
          None
        else {
          val len = result.size - 1
          result.last match {
            case ExpectDigitOrPoint =>
              Some(WideToken(Integer(line.take(len).toLong), len))
            case ExpectDigitOrEAfterPoint | ExpectDigitAfterE =>
              Some(WideToken(Floating(line.take(len).toDouble), len))
            case _ =>
              None
          }
        }
      }

  sealed trait LiteralParseMode

  case object ExpectLetter extends LiteralParseMode

  case object ExpectLetterOrDigit extends LiteralParseMode

  case object ExpectLetterOrDigitAfterHyphen extends LiteralParseMode

  case object LiteralParseFinished extends LiteralParseMode

  def findLiteral(line: String): Option[Token] =
    line
      .scanLeft(ExpectLetter: LiteralParseMode) {
        case (ExpectLetter, char: Char) =>
          if (isLetter(char)) ExpectLetterOrDigit else LiteralParseFinished
        case (ExpectLetterOrDigit | ExpectLetterOrDigitAfterHyphen, char: Char) =>
          if (isLetterOrDigit(char)) ExpectLetterOrDigit else if (isHyphen(char)) ExpectLetterOrDigitAfterHyphen else LiteralParseFinished
        case _ =>
          LiteralParseFinished
      }
      .takeWhile(_ != LiteralParseFinished)
      .pipe { result =>
        if (result.isEmpty)
          None
        else
          result.last match {
            case ExpectLetterOrDigit =>
              Some(Identifier(line.take(result.size - 1)))
            case _ =>
              None
          }
      }

  private def isDigit(char: Char): Boolean = char >= '0' && char <= '9'

  private def isDigitOrMinus(char: Char): Boolean = isDigit(char) || char == '-'

  private def isLetter(char: Char): Boolean = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

  private def isLetterOrDigit(char: Char): Boolean = isLetter(char) || isDigit(char)

  private def isHyphen(char: Char): Boolean = char == '-' || char == '_'

  private def splitBySeparator[T](list: List[T], sep: T): List[List[T]] =
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
          (tokens, Nil)
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

  case class WideToken(wrapped: Token, override val length: Int) extends Token {
    override def value: String = wrapped.value
  }

  sealed trait Comment extends Token

  case object SingleCommentLiteral extends Comment {
    override def value: String = "//"
  }

  case object CommentStartLiteral extends Comment {
    override def value: String = "/*"
  }

  case class SingleComment(string: String) extends Comment {
    override def value: String = s"//$string"
  }

  case class CommentStart(string: String) extends Comment {
    override def value: String = s"/*$string"
  }

  case class CommentLine(string: String) extends Comment {
    override def value: String = string
  }

  case class CommentInline(string: String) extends Comment {
    override def value: String = s"/*$string*/"
  }

  case class CommentEnd(string: String) extends Comment {
    override def value: String = s"$string*/"
  }

  case object TripleQuote extends Token {
    override def value: String = "\"\"\""
  }

  case class MultilineStringPart(value: String) extends Token

  case class MultilineString(value: String) extends Token

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
    val `//`: Token = SingleCommentLiteral
    val `/*`: Token = CommentStartLiteral
    val `"""`: Token = TripleQuote
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
      `//`,
      `/*`,
      `"""`,
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

  case class StringLiteral(override val value: String) extends ValueToken

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

  case class DefinitionReturnTypeMismatch(name: Identifier, expected: Type, got: Type) extends EvaluationErrorToken {
    override def value: String = s"def ${name.value} expected: ${expected}, got: ${got}"
  }

  case object DivisionByZero extends EvaluationErrorToken {
    override val value: String = "0 / 0"
  }

  case class UnexpectedIdentifier(token: Token) extends EvaluationErrorToken {
    override def value: String = token.value
  }

  case class UnexpectedIdentifierInBlock(token: Token) extends EvaluationErrorToken {
    override def value: String = token.value
  }

  case class UnexpectedIdentifierAfterDot(element: Element) extends EvaluationErrorToken {
    override def value: String = element.name.value
  }

  case class UnexpectedEnumValueAfterDot(element: Element, enumValue: Identifier) extends EvaluationErrorToken {
    override def value: String = element.name.value
  }

  case class NotStatic(className: Identifier, identifier: Identifier) extends EvaluationErrorToken {
    override def value: String = s"${className.value} ${identifier.value}"
  }

  case class UnexpectedEvaluation(acc: List[EvaluatedToken], token: EvaluatedToken) extends EvaluationErrorToken {
    override def value: String = s"${token.value}, ${acc.map(_.value)}"
  }

  case class TooManyEvaluatedTokens(tokens: List[EvaluatedToken]) extends EvaluationErrorToken {
    override def value: String = tokens.map(_.value).mkString(", ")
  }

  case object Pass extends EvaluatedToken {
    override val value: String = "pass"
  }

  sealed trait EvaluatedIdentifier extends EvaluatedToken {
    def identifier: Identifier
  }

  sealed trait EvaluatedClass extends EvaluatedIdentifier {
    def cls: elements.Class
    override def identifier: Identifier = cls.name
  }

  case class ClassInstance(cls: elements.Class, values: List[EvaluatedToken]) extends EvaluatedClass {
    override def value: String = s"instance ${cls.name.value}($values)"
  }

  case class ClassStatic(cls: elements.Class) extends EvaluatedClass {
    override def value: String = s"static ${cls.name.value}"
  }

  case class EvaluatedAssignment(asg: elements.Assignment) extends EvaluatedIdentifier {
    override def value: String = s"assignment ${asg.name.value}"
    override def identifier: Identifier = asg.name
  }

  sealed trait EvaluatedEnum extends EvaluatedIdentifier {
    def enm: elements.Enum
    override def identifier: Identifier = enm.name
  }

  case class EnumInstance(enm: elements.Enum, enumValue: Identifier) extends EvaluatedEnum {
    override def value: String = s"instance ${enm.name.value}(${enumValue.value})"
  }

  case class EnumStatic(enm: elements.Enum) extends EvaluatedEnum {
    override def value: String = s"static ${enm.name.value}"
  }

  case class CallDefinition(definition: elements.Definition, values: List[EvaluatedToken], ec: Option[EvaluatedClass]) extends EvaluatedToken {
    override def value: String = s"call ${definition.name.value}($values)"
  }

  case class EvaluatedDot(ec: EvaluatedClass, child: EvaluatedToken) extends EvaluatedToken {
    override def value: String = s"(${ec.value}).(${child.value})"
  }

  case class EvaluatedUnaryOperator(op: Operators, a: EvaluatedToken) extends EvaluatedToken {
    override def value: String = s"${op.value}${a.value}"
  }

  case class EvaluatedOperator(a: EvaluatedToken, b: EvaluatedToken, op: Operators) extends EvaluatedToken {
    override def value: String = s"${a.value}${op.value}${b.value}"
  }

}
