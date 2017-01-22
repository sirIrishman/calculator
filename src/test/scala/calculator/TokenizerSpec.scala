package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class TokenizerSpec extends FlatSpec {
  behavior of "Tokenizer Tokenize method"

  // single number
  it should "process single zero" in {
    assert(Tokenizer.tokenize("0") == Left(List(new Token("0", TokenKind.Number, 0))))
    assert(Tokenizer.tokenize("0.0") == Left(List(new Token("0.0", TokenKind.Number, 0))))
  }

  it should "process single integer number" in {
    assert(Tokenizer.tokenize("12") == Left(List(new Token("12", TokenKind.Number, 0))))
    assert(Tokenizer.tokenize("-12") == Left(List(new Token("-12", TokenKind.Number, 0))))
    assert(Tokenizer.tokenize(" 12  ") == Left(List(new Token("12", TokenKind.Number, 1))))
    assert(Tokenizer.tokenize(" -12  ") == Left(List(new Token("-12", TokenKind.Number, 1))))
    assert(Tokenizer.tokenize("  12 ") == Left(List(new Token("12", TokenKind.Number, 2))))
    assert(Tokenizer.tokenize("  -12 ") == Left(List(new Token("-12", TokenKind.Number, 2))))
    assert(Tokenizer.tokenize("(654)") ==
      Left(List(
        new Token("(", TokenKind.Operator, 0),
        new Token("654", TokenKind.Number, 1),
        new Token(")", TokenKind.Operator, 4)
      )))
    assert(Tokenizer.tokenize("(-654)") ==
      Left(List(
        new Token("(", TokenKind.Operator, 0),
        new Token("-654", TokenKind.Number, 1),
        new Token(")", TokenKind.Operator, 5)
      )))
    assert(Tokenizer.tokenize(" (12)   ") ==
      Left(List(
        new Token("(", TokenKind.Operator, 1),
        new Token("12", TokenKind.Number, 2),
        new Token(")", TokenKind.Operator, 4)
      )))
  }

  it should "process single float number" in {
    assert(Tokenizer.tokenize("5.46") == Left(List(new Token("5.46", TokenKind.Number, 0))))
    assert(Tokenizer.tokenize("-5.46") == Left(List(new Token("-5.46", TokenKind.Number, 0))))
    assert(Tokenizer.tokenize("(94.0551)") ==
      Left(List(
        new Token("(", TokenKind.Operator, 0),
        new Token("94.0551", TokenKind.Number, 1),
        new Token(")", TokenKind.Operator, 8)
      )))

    assert(Tokenizer.tokenize("(-94.0551)") ==
      Left(List(
        new Token("(", TokenKind.Operator, 0),
        new Token("-94.0551", TokenKind.Number, 1),
        new Token(")", TokenKind.Operator, 9)
      )))
  }

  // numbers with operators
  it should "process integer numbers with operators" in {
    assert(Tokenizer.tokenize("0+11*222") ==
      Left(List(
        new Token("0", TokenKind.Number, 0),
        new Token("+", TokenKind.Operator, 1),
        new Token("11", TokenKind.Number, 2),
        new Token("*", TokenKind.Operator, 4),
        new Token("222", TokenKind.Number, 5)
      )))

    assert(Tokenizer.tokenize("0 + 11 * 222") ==
      Left(List(
        new Token("0", TokenKind.Number, 0),
        new Token("+", TokenKind.Operator, 2),
        new Token("11", TokenKind.Number, 4),
        new Token("*", TokenKind.Operator, 7),
        new Token("222", TokenKind.Number, 9)
      ))
    )
  }

  it should "process float numbers with operators" in {
    assert(Tokenizer.tokenize("11.99/4.0-3.5") ==
      Left(List(
        new Token("11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 5),
        new Token("4.0", TokenKind.Number, 6),
        new Token("-", TokenKind.Operator, 9),
        new Token("3.5", TokenKind.Number, 10)
      )))

    assert(Tokenizer.tokenize("11.99 / 4.0 - 3.5") ==
      Left(List(
        new Token("11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 6),
        new Token("4.0", TokenKind.Number, 8),
        new Token("-", TokenKind.Operator, 12),
        new Token("3.5", TokenKind.Number, 14)
      )))
  }

  it should "process numbers with operators and parentheses" in {
    assert(Tokenizer.tokenize("11.99/((4.0-3)-1)") ==
      Left(List(
        new Token("11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 5),
        new Token("(", TokenKind.Operator, 6),
        new Token("(", TokenKind.Operator, 7),
        new Token("4.0", TokenKind.Number, 8),
        new Token("-", TokenKind.Operator, 11),
        new Token("3", TokenKind.Number, 12),
        new Token(")", TokenKind.Operator, 13),
        new Token("-", TokenKind.Operator, 14),
        new Token("1", TokenKind.Number, 15),
        new Token(")", TokenKind.Operator, 16)
      )))

    assert(Tokenizer.tokenize("11.99 / ( ( 4.0 - 3 ) - 1 )") ==
      Left(List(
        new Token("11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 6),
        new Token("(", TokenKind.Operator, 8),
        new Token("(", TokenKind.Operator, 10),
        new Token("4.0", TokenKind.Number, 12),
        new Token("-", TokenKind.Operator, 16),
        new Token("3", TokenKind.Number, 18),
        new Token(")", TokenKind.Operator, 20),
        new Token("-", TokenKind.Operator, 22),
        new Token("1", TokenKind.Number, 24),
        new Token(")", TokenKind.Operator, 26)
      )))

    assert(Tokenizer.tokenize("-11.99/((-4.0*-3)-1)") ==
      Left(List(
        new Token("-11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 6),
        new Token("(", TokenKind.Operator, 7),
        new Token("(", TokenKind.Operator, 8),
        new Token("-4.0", TokenKind.Number, 9),
        new Token("*", TokenKind.Operator, 13),
        new Token("-3", TokenKind.Number, 14),
        new Token(")", TokenKind.Operator, 16),
        new Token("-", TokenKind.Operator, 17),
        new Token("1", TokenKind.Number, 18),
        new Token(")", TokenKind.Operator, 19)
      )))

    assert(Tokenizer.tokenize("-11.99 / ( ( -4.0 * -3 ) - 1 )") ==
      Left(List(
        new Token("-11.99", TokenKind.Number, 0),
        new Token("/", TokenKind.Operator, 7),
        new Token("(", TokenKind.Operator, 9),
        new Token("(", TokenKind.Operator, 11),
        new Token("-4.0", TokenKind.Number, 13),
        new Token("*", TokenKind.Operator, 18),
        new Token("-3", TokenKind.Number, 20),
        new Token(")", TokenKind.Operator, 23),
        new Token("-", TokenKind.Operator, 25),
        new Token("1", TokenKind.Number, 27),
        new Token(")", TokenKind.Operator, 29)
      )))

  }
  //invalid expressions
  it should "return an error when tokens are unrecognizable" in {
    assert(Tokenizer.tokenize("12.34.56") == Right(new ExpressionError("Failed to parse '12.34.56':0 token")))
    assert(Tokenizer.tokenize("12..34") == Right(new ExpressionError("Failed to parse '12.':0 token")))
    assert(Tokenizer.tokenize(".12") == Right(new ExpressionError("Failed to parse '.12':0 token")))
  }
}
