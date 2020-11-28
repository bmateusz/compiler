package compiler

import compiler.Errors.{CompilerError, UnparsedTokens}
import compiler.Tokens.Token

import scala.util.chaining.scalaUtilChainingOps

case class Result[+A](value: Either[List[CompilerError], A],
                      rest: List[Token]) {
  def map[B](f: A => B): Result[B] =
    value match {
      case Left(value) => Result(value, rest)
      case Right(value) => Result(f(value), rest)
    }

  def flatMap[B](f: (A, List[Token]) => Result[B]): Result[B] =
    value match {
      case Left(value) => Result(value, rest)
      case Right(value) => f(value, rest)
    }

  def finishedParsingTokens(): Result[A] =
    if (rest.isEmpty) {
      this
    } else {
      val error = UnparsedTokens(rest)
      value match {
        case Left(value) => Result(value :+ error, List.empty)
        case Right(value) => Result(Left(List(error)), List.empty)
      }
    }
}

object Result {

  implicit class ResultOps[+A](result: List[Result[A]]) {
    def mapEither[B](mapLefts: List[List[CompilerError]] => List[CompilerError],
                     mapRights: List[A] => B): Either[List[CompilerError], B] =
      result
        .map(_.value)
        .partitionMap(identity)
        .pipe { case (lefts, rights) =>
          if (lefts.isEmpty) {
            Right(mapRights(rights))
          } else {
            Left(mapLefts(lefts))
          }
        }
  }

  def apply[A](value: Either[List[CompilerError], A], rest: List[Token]): Result[A] =
    new Result(value, rest)

  def apply[A](success: A): Result[A] =
    new Result(Right(success), Nil)

  def apply[A](success: A, rest: List[Token]): Result[A] =
    new Result(Right(success), rest)

  def apply[A](error: List[CompilerError]): Result[A] =
    new Result(Left(error), Nil)

  def apply[A](error: List[CompilerError], rest: List[Token]): Result[A] =
    new Result(Left(error), rest)

  def apply[A](error: CompilerError): Result[A] =
    new Result(Left(List(error)), Nil)

  def apply[A](error: CompilerError, rest: List[Token]): Result[A] =
    new Result(Left(List(error)), rest)

}
