package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class TokenizerSpec extends FlatSpec {
  behavior of "Tokenizer Tokenize method"

  // single number
  it should "process single zero" in {
    assert(Tokenizer.Tokenize("0") == Left(List(new Token("0", TokenType.Number, 0))))
    assert(Tokenizer.Tokenize("0.0") == Left(List(new Token("0.0", TokenType.Number, 0))))
  }

  it should "process single integer number" in {
    assert(Tokenizer.Tokenize("12") == Left(List(new Token("12", TokenType.Number, 0))))
    assert(Tokenizer.Tokenize("-12") == Left(List(new Token("-12", TokenType.Number, 0))))
    assert(Tokenizer.Tokenize(" 12  ") == Left(List(new Token("12", TokenType.Number, 1))))
    assert(Tokenizer.Tokenize(" -12  ") == Left(List(new Token("-12", TokenType.Number, 1))))
    assert(Tokenizer.Tokenize("  12 ") == Left(List(new Token("12", TokenType.Number, 2))))
    assert(Tokenizer.Tokenize("  -12 ") == Left(List(new Token("-12", TokenType.Number, 2))))
    assert(Tokenizer.Tokenize("(654)") ==
      Left(List(
        new Token("(", TokenType.Operator, 0),
        new Token("654", TokenType.Number, 1),
        new Token(")", TokenType.Operator, 4)
      )))
    assert(Tokenizer.Tokenize("(-654)") ==
      Left(List(
        new Token("(", TokenType.Operator, 0),
        new Token("-654", TokenType.Number, 1),
        new Token(")", TokenType.Operator, 5)
      )))
    assert(Tokenizer.Tokenize(" (12)   ") ==
      Left(List(
        new Token("(", TokenType.Operator, 1),
        new Token("12", TokenType.Number, 2),
        new Token(")", TokenType.Operator, 4)
      )))
  }

  it should "process single float number" in {
    assert(Tokenizer.Tokenize("5.46") == Left(List(new Token("5.46", TokenType.Number, 0))))
    assert(Tokenizer.Tokenize("-5.46") == Left(List(new Token("-5.46", TokenType.Number, 0))))
    assert(Tokenizer.Tokenize("(94.0551)") ==
      Left(List(
        new Token("(", TokenType.Operator, 0),
        new Token("94.0551", TokenType.Number, 1),
        new Token(")", TokenType.Operator, 8)
      )))

    assert(Tokenizer.Tokenize("(-94.0551)") ==
      Left(List(
        new Token("(", TokenType.Operator, 0),
        new Token("-94.0551", TokenType.Number, 1),
        new Token(")", TokenType.Operator, 9)
      )))
  }

  // numbers with operators
  it should "process integer numbers with operators" in {
    assert(Tokenizer.Tokenize("0+11*222") ==
      Left(List(
        new Token("0", TokenType.Number, 0),
        new Token("+", TokenType.Operator, 1),
        new Token("11", TokenType.Number, 2),
        new Token("*", TokenType.Operator, 4),
        new Token("222", TokenType.Number, 5)
      )))

    assert(Tokenizer.Tokenize("0 + 11 * 222") ==
      Left(List(
        new Token("0", TokenType.Number, 0),
        new Token("+", TokenType.Operator, 2),
        new Token("11", TokenType.Number, 4),
        new Token("*", TokenType.Operator, 7),
        new Token("222", TokenType.Number, 9)
      ))
    )
  }

  it should "process float numbers with operators" in {
    assert(Tokenizer.Tokenize("11.99/4.0-3.5") ==
      Left(List(
        new Token("11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 5),
        new Token("4.0", TokenType.Number, 6),
        new Token("-", TokenType.Operator, 9),
        new Token("3.5", TokenType.Number, 10)
      )))

    assert(Tokenizer.Tokenize("11.99 / 4.0 - 3.5") ==
      Left(List(
        new Token("11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 6),
        new Token("4.0", TokenType.Number, 8),
        new Token("-", TokenType.Operator, 12),
        new Token("3.5", TokenType.Number, 14)
      )))
  }

  it should "process numbers with operators and parentheses" in {
    assert(Tokenizer.Tokenize("11.99/((4.0-3)-1)") ==
      Left(List(
        new Token("11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 5),
        new Token("(", TokenType.Operator, 6),
        new Token("(", TokenType.Operator, 7),
        new Token("4.0", TokenType.Number, 8),
        new Token("-", TokenType.Operator, 11),
        new Token("3", TokenType.Number, 12),
        new Token(")", TokenType.Operator, 13),
        new Token("-", TokenType.Operator, 14),
        new Token("1", TokenType.Number, 15),
        new Token(")", TokenType.Operator, 16)
      )))

    assert(Tokenizer.Tokenize("11.99 / ( ( 4.0 - 3 ) - 1 )") ==
      Left(List(
        new Token("11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 6),
        new Token("(", TokenType.Operator, 8),
        new Token("(", TokenType.Operator, 10),
        new Token("4.0", TokenType.Number, 12),
        new Token("-", TokenType.Operator, 16),
        new Token("3", TokenType.Number, 18),
        new Token(")", TokenType.Operator, 20),
        new Token("-", TokenType.Operator, 22),
        new Token("1", TokenType.Number, 24),
        new Token(")", TokenType.Operator, 26)
      )))

    assert(Tokenizer.Tokenize("-11.99/((-4.0*-3)-1)") ==
      Left(List(
        new Token("-11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 6),
        new Token("(", TokenType.Operator, 7),
        new Token("(", TokenType.Operator, 8),
        new Token("-4.0", TokenType.Number, 9),
        new Token("*", TokenType.Operator, 13),
        new Token("-3", TokenType.Number, 14),
        new Token(")", TokenType.Operator, 16),
        new Token("-", TokenType.Operator, 17),
        new Token("1", TokenType.Number, 18),
        new Token(")", TokenType.Operator, 19)
      )))

    assert(Tokenizer.Tokenize("-11.99 / ( ( -4.0 * -3 ) - 1 )") ==
      Left(List(
        new Token("-11.99", TokenType.Number, 0),
        new Token("/", TokenType.Operator, 7),
        new Token("(", TokenType.Operator, 9),
        new Token("(", TokenType.Operator, 11),
        new Token("-4.0", TokenType.Number, 13),
        new Token("*", TokenType.Operator, 18),
        new Token("-3", TokenType.Number, 20),
        new Token(")", TokenType.Operator, 23),
        new Token("-", TokenType.Operator, 25),
        new Token("1", TokenType.Number, 27),
        new Token(")", TokenType.Operator, 29)
      )))
  }
}
