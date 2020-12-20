package compiler.elements

import compiler.Errors.Redefinition
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.Tokens.{Class, Def, Enum, Equals, Identifier, Indentation, Token}
import compiler.{Expression, Result}

import scala.annotation.tailrec

case class Block(elements: List[Element],
                 expression: Option[Expression]) {
  def sortedElements: List[Element] =
    elements
      .sortBy(_.name.value)

  def add(element: Element, rest: List[Token]): Result[Block] =
    if (elements.exists(_.name.value == element.name.value))
      Result(Redefinition(element.name.value), rest)
    else
      Result(
        copy(
          elements = elements :+ element
        ),
        rest
      )

  def set(expression: Expression): Block =
    copy(expression = Some(expression))

  def get(identifier: Identifier): Option[Element] =
    elements.find(_.name.value == identifier.value)

  def evaluate(parent: Block = Block.empty, rest: List[Token] = List.empty, em: EvaluationMode = FullEvaluation): Result[Block] =
    elements.foldLeft(Result(parent, rest)) {
      case (Result(Right(block), rest), curr) =>
        curr
          .evaluate(block, rest, em)
          .flatMap {
            case (newElement, rest) =>
              block.add(newElement, rest)
          }
      case (left@Result(_, _), curr) =>
        left
    }

  def filtered: Block =
    copy(
      elements.filter {
        case Assignment(_, _) => false
        case _ => true
      },
      None
    )
}

object Block {

  val empty: Block =
    Block(List.empty, None)

  def parse(result: Result[Block], indentation: List[Indentation]): Result[Block] =
    result.flatMap((block, rest) => parse(rest, block, indentation))

  @tailrec
  def parse(tokens: List[Token], block: Block, indentation: List[Indentation]): Result[Block] =
    tokens match {
      case (current: Indentation) :: xs =>
        indentation match {
          case Indentation(length) :: _ if current.length == length =>
            parse(xs, block, indentation)
          case Indentation(length) :: _ if current.length > length =>
            parse(xs, block, indentation :+ current)
          case Indentation(length) :: Nil if current.length < length =>
            Result(block, tokens)
          case Indentation(length) :: indentationRest if current.length < length =>
            parse(xs, block, indentationRest)
          case Nil =>
            parse(xs, block, List(current))
        }
      case Def :: xs =>
        Definition
          .parse(xs)
          .flatMap { (definition, rest) =>
            parse(block.add(definition, rest), indentation)
          }
      case Class :: xs =>
        compiler.elements.Class
          .parse(xs)
          .flatMap { (cls, rest) =>
            parse(block.add(cls, rest), indentation)
          }
      case Enum :: xs =>
        compiler.elements.Enum
          .parse(xs)
          .flatMap { (enm, rest) =>
            parse(block.add(enm, rest), indentation)
          }
      case (identifier: Identifier) :: Equals :: xs =>
        Assignment
          .parse(identifier, xs)
          .flatMap { (assignment, rest) =>
            parse(block.add(assignment, rest), indentation)
          }
      case Nil =>
        Result(block)
      case others =>
        Expression
          .parse(others)
          .flatMap { (expr, rest) =>
            Result(block.set(expr), rest)
          }
    }

}
