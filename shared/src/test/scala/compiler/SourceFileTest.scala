package compiler

import compiler.Errors.{CommentNotClosed, InvalidToken}
import compiler.Tokens.{CommentEnd, CommentInline, CommentLine, CommentStart, Indentation, Integer, MultilineString, MultilineStringPart}

class SourceFileTest extends CompilerSpecs {

  it should "tokenize empty file" in {
    val source = parseSuccess("")
    assert(source.tokens === Nil)
  }

  it should "tokenize multiple lines" in {
    val source = parseSuccess("x = 1\ny = 2\nz = 3")
    assert(source.tokens.length === 12)
  }

  it should "report token error" in {
    val errors = parseError("x = 1\n∂ = 2\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 2")))
  }

  it should "report token error truncated" in {
    val errors = parseError("x = 1\n∂ = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9\nz = 3")
    assert(errors === List(InvalidToken(1, 0, "∂ = 1 + 2 + 3 + 4 + 5 + 6 +...")))
  }

  it should "ignore multi line comment" in {
    val source = parseSuccess("1 /* one\n two \n */ 2 \n /* \n break */ 3 /* three */ 4")
    assert(source.tokens === List(
      Indentation(0),
      Integer(1),
      CommentStart("/* one"),
      CommentLine(" two "),
      CommentEnd(" */"),
      Indentation(1),
      Integer(2),
      Indentation(1),
      CommentStart("/* "),
      CommentEnd(" break */"),
      Indentation(1),
      Integer(3),
      CommentInline(" three "),
      Integer(4)
    ))
  }

  it should "report error on unclosed multi line comment" in {
    val errors = parseError("1 /*")
    assert(errors === List(
      CommentNotClosed()
    ))
  }

  it should "parse multiline string" in {
    val source = parseSuccess("1 \"\"\"hello\n \nworld\"\"\" 2")
    assert(source.tokens === List(
      Indentation(0),
      Integer(1),
      MultilineStringPart("\"\"\"hello"),
      MultilineStringPart(" "),
      MultilineString("world\"\"\""),
      Indentation(1),
      Integer(2)
    ))
  }
}
