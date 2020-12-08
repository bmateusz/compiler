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

  sealed trait Value {
    def half: Value

    def op(operator: Operator, other: () => Value): Value
  }

  case class Error(string: String) extends Value {
    override def half: Value = this
    override def op(operator: Operator, other: () => Value): Value = this
  }

  case class IntValue(a: Long) extends Value {
    override def half: Value = IntValue(a / 2)
    override def op(operator: Operator, other: () => Value): Value =
      other() match {
        case b: Error => b
        case IntValue(b) =>
          operator match {
            case Add => IntValue(a + b)
            case Subtract => IntValue(a - b)
            case Multiple => IntValue(a * b)
            case Divide if b != 0 => IntValue(a / b)
            case Divide => Error("Division by zero")
          }
        case FloatValue(b) =>
          operator match {
            case Add => FloatValue(a + b)
            case Subtract => FloatValue(a - b)
            case Multiple => FloatValue(a * b)
            case Divide => FloatValue(a / b)
          }
      }
  }

  case class FloatValue(a: Double) extends Value {
    override def half: Value = FloatValue(a / 2)
    override def op(operator: Operator, other: () => Value): Value =
      other() match {
        case b: Error => b
        case IntValue(b) =>
          operator match {
            case Add => FloatValue(a + b)
            case Subtract => FloatValue(a - b)
            case Multiple => FloatValue(a * b)
            case Divide => FloatValue(a / b)
          }
        case FloatValue(b) =>
          operator match {
            case Add => FloatValue(a + b)
            case Subtract => FloatValue(a - b)
            case Multiple => FloatValue(a * b)
            case Divide => FloatValue(a / b)
          }
      }
  }

  sealed trait Tree {
    def evaluate: Value
  }

  case class Node(left: Tree, right: Tree, operator: Operator) extends Tree {
    override def toString: String = s"($left$operator$right)"

    override def evaluate: Value =
      left.evaluate.op(operator, () => right.evaluate)
  }

  case class Leaf(value: Value) extends Tree {
    override val toString: String = value match {
      case Error(string) => string
      case IntValue(int) => int.toString
      case FloatValue(float) => float.toString
    }

    override val evaluate: Value = value
  }

  val genOperator: Gen[Operator] =
    oneOf(
      Add,
      Subtract,
      Multiple,
      Divide
    )

  val genLeaf: Gen[Leaf] = Gen.frequency(
    (1, Gen.long.map(a => Leaf(IntValue(a)))),
    (1, Gen.double.map(a => Leaf(FloatValue(a))))
  )

  val genNode: Gen[Node] = for {
    left <- Gen.sized(h => Gen.resize(h / 2, genTree))
    right <- Gen.sized(h => Gen.resize(h / 2, genTree))
    operator <- genOperator
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
        List(Leaf(IntValue(1)), Leaf(FloatValue(1.0)))
    case Leaf(value) =>
      shrink(value).map(v => Leaf(v.half))
  }

  implicit val arbOperator: Arbitrary[Operator] = Arbitrary(genOperator)

}
