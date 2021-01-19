package compiler.elements

import compiler.Errors.{CompilerError, ExpectedColon, ExpectedIdentifier, ExpectedLeftParenthesis, ExpectedRightParenthesis, ExpectedType, NotUniqueParameters}
import compiler.Tokens.{TokenListExtension, _}
import compiler.Types.Type
import compiler.elements.Parameters.Parameter
import compiler.{Result, Types}

import scala.annotation.tailrec

case class Parameters(values: List[Parameter]) {
  def addParameter(name: String, typ: String): Parameters =
    copy(values = values :+ Parameter(Identifier(name), Types.parse(typ)))

  def notUniqueIdentifiers(): List[String] =
    Identifier.notUniqueIdentifiers(values.map(_.identifier))

  def findParameter(identifier: Identifier): Option[(Parameter, Int)] =
    values.zipWithIndex.find(_._1.identifier == identifier)
}

object Parameters {

  case class Parameter(identifier: Identifier, typ: Type)

  val empty: Parameters = Parameters(Nil)

  @tailrec
  def parse(tokens: List[Token]): Result[(Parameters, Option[Type])] =
    tokens match {
      case Indentation(_) :: LeftParenthesis :: xs =>
        parse(LeftParenthesis :: xs)
      case Indentation(_) :: Colon :: xs =>
        parse(Colon :: xs)
      case LeftParenthesis :: xs =>
        val (left, right) = xs.spanMatchingRightParenthesis()
        right match {
          case RightParenthesis :: Colon :: Identifier(returnType) :: rest =>
            Result(
              parseParameters(left, empty).map((_, Some(Types.parse(returnType)))),
              rest
            )
          case RightParenthesis :: rest =>
            Result(parseParameters(left, empty).map((_, None)), rest)
          case _ =>
            Result(ExpectedRightParenthesis(None))
        }
      case Colon :: Identifier(returnType) :: rest =>
        Result((Parameters.empty, Some(Types.parse(returnType))), rest)
      case rest =>
        Result((Parameters.empty, None), rest)
    }

  @tailrec
  def parseParameters(tokens: List[Token], pl: Parameters): Either[List[CompilerError], Parameters] =
    tokens match {
      case Indentation(_) :: xs =>
        parseParameters(xs, pl)
      case Identifier(name) :: Colon :: Identifier(typ) :: Indentation(_) :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Comma :: rest =>
        parseParameters(rest, pl.addParameter(name, typ))
      case Identifier(name) :: Colon :: Identifier(typ) :: Nil =>
        checkParameters(pl.addParameter(name, typ))
      case Nil =>
        checkParameters(pl)
      case Identifier(name) :: Colon :: tokens =>
        Left(List(ExpectedType(tokens.headOption)))
      case Identifier(name) :: tokens =>
        Left(List(ExpectedColon(tokens.headOption)))
      case tokens =>
        Left(List(ExpectedIdentifier(tokens.headOption)))
    }

  def checkParameters(parameters: Parameters): Either[List[CompilerError], Parameters] =
    parameters.notUniqueIdentifiers() match {
      case Nil => Right(parameters)
      case xs => Left(List(NotUniqueParameters(xs)))
    }

}
