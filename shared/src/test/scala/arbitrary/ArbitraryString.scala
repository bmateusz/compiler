package arbitrary

import org.scalacheck.Shrink.shrink
import org.scalacheck.{Arbitrary, Gen, Shrink}

object ArbitraryString {

  import Operators._

  object Operators {

    sealed trait Operator

    case object Add extends Operator {
      override def toString: String = "+"
    }

  }

  sealed trait Value {
    def half: Value

    def op(operator: Operator, other: () => Value): Value
  }

  case class StringValue(a: String) extends Value {
    override def half: Value = StringValue(a.take(a.length / 2))

    override def op(operator: Operator, other: () => Value): Value =
      other() match {
        case StringValue(b) =>
          operator match {
            case Add => StringValue(a + b)
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
      case StringValue(string) => s""""$string""""
    }

    override val evaluate: Value = value
  }

  val genOperator: Gen[Operator] =
    Gen.const(
      Add
    )

  def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString().tail.init
  }

  val genLeaf: Gen[Leaf] = Gen.frequency(
    (1, Gen.asciiPrintableStr.map(a => Leaf(StringValue(escape(a))))),
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
        List(Leaf(StringValue("")))
    case Leaf(value) =>
      shrink(value).map(v => Leaf(v.half))
  }

  implicit val arbOperator: Arbitrary[Operator] = Arbitrary(genOperator)

}
