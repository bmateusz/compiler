package compiler.elements

import compiler.Errors.Redefinition
import compiler.Expression.{EvaluationMode, FullEvaluation}
import compiler.Tokens.{Class, Colon, Comment, Def, Enum, Equals, Identifier, Indentation, Token}
import compiler.{Expression, Result}

import scala.annotation.tailrec

case class Block(elements: List[Element],
                 expression: Option[Expression],
                 parent: Option[Block] = None) {
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

  def setExpression(expression: Expression): Block =
    copy(expression = Some(expression))

  def setParent(block: Block): Block =
    copy(parent = Some(block))

  lazy val identifierMap: Map[String, Element] =
    elements
      .flatMap {
        case enm: Enum =>
          enm.allElements
        case other =>
          List(other.name.value -> other)
      }
      .toMap


  def get(identifier: Identifier): Option[Element] =
    identifierMap.get(identifier.value)
      .orElse(parent.flatMap(_.get(identifier)))

  def evaluate(parent: Block = Block.empty, rest: List[Token] = Nil, em: EvaluationMode = FullEvaluation): Result[Block] =
    elements.foldLeft(Result(parent, rest)) {
      case (Result(Right(block), rest), curr) =>
        curr
          .evaluate(block, rest, em)
          .flatMap {
            case (newElement, rest) =>
              block.add(newElement, rest)
          }
      case (left@Result(_, _), _) =>
        left
    }
}

object Block {

  val empty: Block =
    Block(Nil, None)

  def parse(result: Result[Block], indentation: Option[Indentation], exprs: Boolean): Result[Block] =
    result.flatMap((block, rest) => parse(rest, block, indentation, exprs))

  @tailrec
  def parse(tokens: List[Token], block: Block, indentation: Option[Indentation], exprs: Boolean): Result[Block] =
    tokens match {
      case (current: Indentation) :: xs =>
        indentation match {
          case Some(Indentation(previousLength)) =>
            if (current.length >= previousLength)
              parse(xs, block, Some(current), exprs)
            else
              finishBlock(block, tokens)
          case None =>
            parse(xs, block, Some(current), exprs)
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
      case (_: Comment) :: xs =>
        parse(xs, block, indentation, exprs)
      case Nil =>
        finishBlock(block, Nil)
      case others =>
        if (exprs)
          Expression
            .parse(others)
            .flatMap { (expr, rest) =>
              finishBlock(block.setExpression(expr), rest)
            }
        else
          finishBlock(block, others)
    }

  private def finishBlock(block: Block, others: List[Token]) =
    Result(block, others)

  private def top(indentations: Option[Indentation]) =
    indentations.getOrElse(Indentation(0))

}
