package compiler.elements

import compiler.Errors.Redefinition
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.Tokens.{Class, Colon, Def, Enum, Equals, Identifier, Indentation, Token}
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
        case Assignment(_, _, _) => false
        case _ => true
      },
      None
    )
}

object Block {

  val empty: Block =
    Block(List.empty, None)

  def parse(result: Result[Block], indentation: List[Indentation], exprs: Boolean): Result[Block] =
    result.flatMap((block, rest) => parse(rest, block, indentation, exprs))

  @tailrec
  def parse(tokens: List[Token], block: Block, indentation: List[Indentation], exprs: Boolean): Result[Block] =
    tokens match {
      case (current: Indentation) :: xs =>
        indentation.lastOption match {
          case Some(Indentation(length)) if current.length == length =>
            parse(xs, block, indentation, exprs)
          case Some(Indentation(length)) if current.length > length =>
            parse(xs, block, indentation :+ current, exprs)
          case Some(Indentation(length)) if current.length < length =>
            if (indentation.size == 1)
              Result(block, tokens)
            else
              parse(xs, block, indentation.filter(current.length < _.length), exprs)
          case None =>
            parse(xs, block, List(current), exprs)
        }
      case Def :: xs =>
        Definition
          .parse(xs, top(indentation))
          .flatMap { (definition, rest) =>
            parse(block.add(definition, rest), indentation, exprs)
          }
      case Class :: xs =>
        compiler.elements.Class
          .parse(xs, top(indentation))
          .flatMap { (cls, rest) =>
            parse(block.add(cls, rest), indentation, exprs)
          }
      case Enum :: xs =>
        compiler.elements.Enum
          .parse(xs)
          .flatMap { (enm, rest) =>
            parse(block.add(enm, rest), indentation, exprs)
          }
      case (identifier: Identifier) :: Colon :: (typ: Identifier) :: Equals :: xs =>
        Assignment
          .parse(identifier, Some(typ), xs)
          .flatMap { (assignment, rest) =>
            parse(block.add(assignment, rest), indentation, exprs)
          }
      case (identifier: Identifier) :: Equals :: xs =>
        Assignment
          .parse(identifier, None, xs)
          .flatMap { (assignment, rest) =>
            parse(block.add(assignment, rest), indentation, exprs)
          }
      case Nil =>
        Result(block)
      case others =>
        if (exprs)
          Expression
            .parse(others)
            .flatMap { (expr, rest) =>
              Result(block.set(expr), rest)
            }
        else
          Result(block, others)
    }

  private def top(indentations: List[Indentation]) =
    indentations.lastOption.getOrElse(Indentation(0))

}
