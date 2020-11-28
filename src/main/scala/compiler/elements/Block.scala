package compiler.elements

import compiler.Errors.Redefinition
import compiler.Tokens.{Class, Def, Enum, Equals, Identifier, Indentation, Token}
import compiler.{Expression, Result}

import scala.annotation.tailrec

case class Block(elements: Map[String, Element],
                 expressions: List[Expression]) {
  def sortedElements: List[Element] =
    elements
      .values
      .toList
      .sortBy(_.name.value)

  def add(element: Element, rest: List[Token]): Result[Block] =
    if (elements.contains(element.name.value))
      Result(Redefinition(element.name.value), rest)
    else
      Result(copy(elements = elements + (element.name.value -> element)), rest)

  def add(expression: Expression): Block =
    copy(expressions = expressions :+ expression)

  def get(identifier: Identifier): Option[Element] =
    elements.get(identifier.value)
}

object Block {

  val empty: Block =
    Block(Map.empty, List.empty)

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
          .parse(others, List.empty, List.empty, None)
          .flatMap { (expr, rest) =>
            Result(block.add(expr), rest)
          }
    }

}
