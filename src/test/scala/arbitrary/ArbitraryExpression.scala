package arbitrary

import org.scalacheck.Gen.oneOf
import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

object ArbitraryExpression {

  import Operators._

  object Operators {

    sealed trait Operator

    case object Add extends Operator {
      override def toString: String = "+"
    }

    case object Subtract extends Operator {
      override def toString: String = "-"
    }

    case object Multiple extends Operator {
      override def toString: String = "*"
    }

    case object Divide extends Operator {
      override def toString: String = "/"
    }

  }

  sealed trait Tree {
    def evaluate: Either[String, Int]
  }

  case class Node(left: Tree, right: Tree, operator: Operator) extends Tree {
    override def toString: String = s"($left$operator$right)"

    override def evaluate: Either[String, Int] = {
      left.evaluate.flatMap { a =>
        right.evaluate.flatMap { b =>
          operator match {
            case Add => Right(a + b)
            case Subtract => Right(a - b)
            case Multiple => Right(a * b)
            case Divide if b != 0 => Right(a / b)
            case Divide => Left("Division by zero")
          }
        }
      }
    }
  }

  case class Leaf(value: Int) extends Tree {
    override val toString: String = value.toString

    override val evaluate: Either[String, Int] = Right(value)
  }

  val arbOperator: Gen[Operator] =
    oneOf(
      Add,
      Subtract,
      Multiple,
      Divide
    )

  val genLeaf: Gen[Leaf] = Gen.chooseNum(-128, 128).map(Leaf)

  val genNode: Gen[Node] = for {
    left <- Gen.sized(h => Gen.resize(h / 2, genTree))
    right <- Gen.sized(h => Gen.resize(h / 2, genTree))
    operator <- arbOperator
  } yield Node(left, right, operator)

  def genTree: Gen[Tree] = Gen.sized { height =>
    if (height <= 0) {
      genLeaf
    } else {
      Gen.oneOf(genLeaf, genNode)
    }
  }

  implicit val arbTree: Arbitrary[Tree] = Arbitrary(genTree)

  implicit val shrinkTree: Shrink[Tree] = Shrink {
    case Node(left, right, operator) =>
      shrink(left).map(Node(_, right, operator)) lazyAppendedAll
        shrink(right).map(Node(left, _, operator)) lazyAppendedAll
        List(Leaf(1))
    case Leaf(value) =>
      shrink(value).map(leafValue => Leaf(leafValue / 2))
  }

}
