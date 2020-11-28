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

  sealed trait IntTree {
    def evaluate: Either[String, Int]
  }

  case class IntNode(left: IntTree, right: IntTree, operator: Operator) extends IntTree {
    override def toString: String = s"($left$operator$right)"

    override def evaluate: Either[String, Int] =
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

  case class IntLeaf(value: Int) extends IntTree {
    override val toString: String = value.toString

    override val evaluate: Either[String, Int] = Right(value)
  }

  sealed trait FloatTree {
    def evaluate: Either[String, Double]
  }

  case class FloatNode(left: FloatTree, right: FloatTree, operator: Operator) extends FloatTree {
    override def toString: String = s"($left$operator$right)"

    override def evaluate: Either[String, Double] =
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

  case class FloatLeaf(value: Double) extends FloatTree {
    override val toString: String = value.toString

    override val evaluate: Either[String, Double] = Right(value)
  }

  val genOperator: Gen[Operator] =
    oneOf(
      Add,
      Subtract,
      Multiple,
      Divide
    )

  val genIntLeaf: Gen[IntLeaf] = Gen.chooseNum(-128, 128).map(IntLeaf)

  val genIntNode: Gen[IntNode] = for {
    left <- Gen.sized(h => Gen.resize(h / 2, genIntTree))
    right <- Gen.sized(h => Gen.resize(h / 2, genIntTree))
    operator <- genOperator
  } yield IntNode(left, right, operator)

  def genIntTree: Gen[IntTree] = Gen.sized { height =>
    if (height <= 0) {
      genIntLeaf
    } else {
      Gen.oneOf(genIntLeaf, genIntNode)
    }
  }

  implicit val arbIntTree: Arbitrary[IntTree] = Arbitrary(genIntTree)

  implicit val shrinkIntTree: Shrink[IntTree] = Shrink {
    case IntNode(left, right, operator) =>
      shrink(left).map(IntNode(_, right, operator)) lazyAppendedAll
        shrink(right).map(IntNode(left, _, operator)) lazyAppendedAll
        List(IntLeaf(1))
    case IntLeaf(value) =>
      shrink(value).map(leafValue => IntLeaf(leafValue / 2))
  }

  val genFloatLeaf: Gen[FloatLeaf] = Gen.double.map(FloatLeaf)

  val genFloatNode: Gen[FloatNode] = for {
    left <- Gen.sized(h => Gen.resize(h / 2, genFloatTree))
    right <- Gen.sized(h => Gen.resize(h / 2, genFloatTree))
    operator <- genOperator
  } yield FloatNode(left, right, operator)

  def genFloatTree: Gen[FloatTree] = Gen.sized { height =>
    if (height <= 0) {
      genFloatLeaf
    } else {
      Gen.oneOf(genFloatLeaf, genFloatNode)
    }
  }

  implicit val arbFloatTree: Arbitrary[FloatTree] = Arbitrary(genFloatTree)

  implicit val shrinkFloatTree: Shrink[FloatTree] = Shrink {
    case FloatNode(left, right, operator) =>
      shrink(left).map(FloatNode(_, right, operator)) lazyAppendedAll
        shrink(right).map(FloatNode(left, _, operator)) lazyAppendedAll
        List(FloatLeaf(1))
    case FloatLeaf(value) =>
      shrink(value).map(leafValue => FloatLeaf(leafValue / 2))
  }

  implicit val arbOperator: Arbitrary[Operator] = Arbitrary(genOperator)

}
