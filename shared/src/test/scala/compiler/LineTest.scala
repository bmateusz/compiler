package compiler

import compiler.Tokens.{Comma, Indentation, Integer, LeftParenthesis, RightParenthesis, TokenListExtension}

class LineTest extends CompilerSpecs {

  it should "find right parenthesis" in {
    val line = parseLineSuccess("() 1")
    assert(line.tokens === List(Indentation(0), LeftParenthesis, RightParenthesis, Integer(1)))
    assert(line.tokens.drop(2).spanMatchingRightParenthesis() === (List(), List(RightParenthesis, Integer(1))))
  }

  it should "find right parenthesis with content" in {
    val line = parseLineSuccess("(22) 1")
    assert(line.tokens === List(Indentation(0), LeftParenthesis, Integer(22), RightParenthesis, Integer(1)))
    assert(line.tokens.drop(2).spanMatchingRightParenthesis() === (List(Integer(22)), List(RightParenthesis, Integer(1))))
  }

  it should "find the correct right parenthesis" in {
    val line = parseLineSuccess("(22 (33) (44)) 1")
    assert(line.tokens === List(Indentation(0), LeftParenthesis, Integer(22), LeftParenthesis, Integer(33), RightParenthesis, LeftParenthesis, Integer(44), RightParenthesis, RightParenthesis, Integer(1)))
    assert(line.tokens.drop(2).spanMatchingRightParenthesis() === (List(Integer(22), LeftParenthesis, Integer(33), RightParenthesis, LeftParenthesis, Integer(44), RightParenthesis), List(RightParenthesis, Integer(1))))
  }

  it should "find right parenthesis if too many" in {
    val line = parseLineSuccess("(22 (33)) 1)")
    assert(line.tokens === List(Indentation(0), LeftParenthesis, Integer(22), LeftParenthesis, Integer(33), RightParenthesis, RightParenthesis, Integer(1), RightParenthesis))
    assert(line.tokens.drop(2).spanMatchingRightParenthesis() === (List(Integer(22), LeftParenthesis, Integer(33), RightParenthesis), List(RightParenthesis, Integer(1), RightParenthesis)))
  }

  it should "not find right parenthesis if unmatched" in {
    val line = parseLineSuccess("(22 (33) 1")
    assert(line.tokens === List(Indentation(0), LeftParenthesis, Integer(22), LeftParenthesis, Integer(33), RightParenthesis, Integer(1)))
    assert(line.tokens.drop(2).spanMatchingRightParenthesis() === (List(Integer(22), LeftParenthesis, Integer(33), RightParenthesis, Integer(1)), List()))
  }

  it should "split by commas" in {
    val line = parseLineSuccess("1, 2, 3")
    assert(line.tokens === List(Indentation(0), Integer(1), Comma, Integer(2), Comma, Integer(3)))
    assert(line.tokens.tail.splitByComma() === List(List(Integer(1)), List(Integer(2)), List(Integer(3))))
  }

  it should "ignore single line comment" in {
    val line = parseLineSuccess("1 // comment")
    assert(line.tokens === List(Indentation(0), Integer(1)))
  }

}
