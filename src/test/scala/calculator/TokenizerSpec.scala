package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.{Tokenizer, Token}

class TokenizerSpec extends FlatSpec {
  behavior of "Tokenizer Tokenize method"

  // single number
  it should "process integer zero" in {
    assert(Tokenizer.Tokenize("0") == Left(List(new Token("0", 0))))
  }

  it should "process float zero" in {
    assert(Tokenizer.Tokenize("0.0") == Left(List(new Token("0.0", 0))))
  }

  it should "process positive integer number" in {
    assert(Tokenizer.Tokenize("12") == Left(List(new Token("12", 0))))
  }

  it should "process positive float number" in {
    assert(Tokenizer.Tokenize("5.46") == Left(List(new Token("5.46", 0))))
  }

  it should "process positive integer number with leading and trailing spaces #1" in {
    assert(Tokenizer.Tokenize(" 12  ") == Left(List(new Token("12", 1))))
  }

  it should "process positive integer number with leading and trailing spaces #2" in {
    assert(Tokenizer.Tokenize("  12 ") == Left(List(new Token("12", 2))))
  }

  it should "process positive integer number inside parentheses" in {
    assert(Tokenizer.Tokenize("(12)") ==
      Left(List(
        new Token("(", 0),
        new Token("12", 1),
        new Token(")", 3)
      )))
  }

  it should "process positive integer number inside parentheses with leading and trailing spaces" in {
    assert(Tokenizer.Tokenize(" (12)   ") ==
      Left(List(
        new Token("(", 1),
        new Token("12", 2),
        new Token(")", 4)
      ))
    )
  }

  // numbers with operators
  it should "process integer numbers with operators" in {
    assert(Tokenizer.Tokenize("0+11*222") ==
      Left(List(
        new Token("0", 0),
        new Token("+", 1),
        new Token("11", 2),
        new Token("*", 4),
        new Token("222", 5)
      ))
    )
  }

  it should "process float numbers with operators" in {
    assert(Tokenizer.Tokenize("11.99/4.0-3.5") ==
      Left(List(
        new Token("11.99", 0),
        new Token("/", 5),
        new Token("4.0", 6),
        new Token("-", 9),
        new Token("3.5", 10)
      ))
    )
  }

  it should "process numbers with operators and parentheses" in {
    assert(Tokenizer.Tokenize("11.99/((4.0-3)-1)") ==
      Left(List(
        new Token("11.99", 0),
        new Token("/", 5),
        new Token("(", 6),
        new Token("(", 7),
        new Token("4.0", 8),
        new Token("-", 11),
        new Token("3", 12),
        new Token(")", 13),
        new Token("-", 14),
        new Token("1", 15),
        new Token(")", 16)
      ))
    )
  }
}
