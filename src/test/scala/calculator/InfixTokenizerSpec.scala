package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class InfixTokenizerSpec extends FlatSpec {
  behavior of "Tokenizer tokenize method"

  // single number
  it should "process single zero" in {
    assert(InfixTokenizer.tokenize("0") == Left(List(NumberToken("0", 0))))
    assert(InfixTokenizer.tokenize("0.0") == Left(List(NumberToken("0.0", 0))))
  }

  it should "process single integer number" in {
    assert(InfixTokenizer.tokenize("12") == Left(List(NumberToken("12", 0))))
    assert(InfixTokenizer.tokenize("-12") == Left(List(NumberToken("-12", 0))))
    assert(InfixTokenizer.tokenize(" 12  ") == Left(List(NumberToken("12", 1))))
    assert(InfixTokenizer.tokenize(" -12  ") == Left(List(NumberToken("-12", 1))))
    assert(InfixTokenizer.tokenize("  12 ") == Left(List(NumberToken("12", 2))))
    assert(InfixTokenizer.tokenize("  -12 ") == Left(List(NumberToken("-12", 2))))
    assert(InfixTokenizer.tokenize("(654)") == Left(List(OperatorToken("(", 0), NumberToken("654", 1), OperatorToken(")", 4))))
    assert(InfixTokenizer.tokenize("(-654)") == Left(List(OperatorToken("(", 0), NumberToken("-654", 1), OperatorToken(")", 5))))
    assert(InfixTokenizer.tokenize(" (12)   ") == Left(List(OperatorToken("(", 1), NumberToken("12", 2), OperatorToken(")", 4))))
  }

  it should "process single float number" in {
    assert(InfixTokenizer.tokenize("5.46") == Left(List(NumberToken("5.46", 0))))
    assert(InfixTokenizer.tokenize("(94.0551)") == Left(List(OperatorToken("(", 0), NumberToken("94.0551", 1), OperatorToken(")", 8))))
    assert(InfixTokenizer.tokenize("-5.46") == Left(List(NumberToken("-5.46", 0))))
    assert(InfixTokenizer.tokenize("(-94.0551)") == Left(List(OperatorToken("(", 0), NumberToken("-94.0551", 1), OperatorToken(")", 9))))
  }

  // constants
  it should "process single constant" in {
    assert(InfixTokenizer.tokenize("Pi") == Left(List(VariableToken("Pi", 0))))
    assert(InfixTokenizer.tokenize("  Pi") == Left(List(VariableToken("Pi", 2))))
    assert(InfixTokenizer.tokenize("  Pi  ") == Left(List(VariableToken("Pi", 2))))
    assert(InfixTokenizer.tokenize("(Pi)") == Left(List(OperatorToken("(", 0), VariableToken("Pi", 1), OperatorToken(")", 3))))

    assert(InfixTokenizer.tokenize("-e") == Left(List(VariableToken("-e", 0))))
    assert(InfixTokenizer.tokenize("  -e") == Left(List(VariableToken("-e", 2))))
    assert(InfixTokenizer.tokenize("  -e  ") == Left(List(VariableToken("-e", 2))))
    assert(InfixTokenizer.tokenize("(-e)") == Left(List(OperatorToken("(", 0), VariableToken("-e", 1), OperatorToken(")", 3))))
  }

  // numbers with operators
  it should "process integer numbers with operators" in {
    assert(InfixTokenizer.tokenize("0+11*222") ==
      Left(List(
        NumberToken("0", 0),
        OperatorToken("+", 1),
        NumberToken("11", 2),
        OperatorToken("*", 4),
        NumberToken("222", 5)
      )))

    assert(InfixTokenizer.tokenize("0 + 11 * 222") ==
      Left(List(
        NumberToken("0", 0),
        OperatorToken("+", 2),
        NumberToken("11", 4),
        OperatorToken("*", 7),
        NumberToken("222", 9)
      ))
    )
  }

  it should "process float numbers with operators" in {
    assert(InfixTokenizer.tokenize("11.99/4.0-3.5") ==
      Left(List(
        NumberToken("11.99", 0),
        OperatorToken("/", 5),
        NumberToken("4.0", 6),
        OperatorToken("-", 9),
        NumberToken("3.5", 10)
      )))

    assert(InfixTokenizer.tokenize("11.99 / 4.0 - 3.5") ==
      Left(List(
        NumberToken("11.99", 0),
        OperatorToken("/", 6),
        NumberToken("4.0", 8),
        OperatorToken("-", 12),
        NumberToken("3.5", 14)
      )))
  }

  it should "process numbers with operators and parentheses" in {
    assert(InfixTokenizer.tokenize("11.99/((4.0-3)-1)") ==
      Left(List(
        NumberToken("11.99", 0),
        OperatorToken("/", 5),
        OperatorToken("(", 6),
        OperatorToken("(", 7),
        NumberToken("4.0", 8),
        OperatorToken("-", 11),
        NumberToken("3", 12),
        OperatorToken(")", 13),
        OperatorToken("-", 14),
        NumberToken("1", 15),
        OperatorToken(")", 16)
      )))

    assert(InfixTokenizer.tokenize("11.99 / ( ( 4.0 - Pi ) - 1 )") ==
      Left(List(
        NumberToken("11.99", 0),
        OperatorToken("/", 6),
        OperatorToken("(", 8),
        OperatorToken("(", 10),
        NumberToken("4.0", 12),
        OperatorToken("-", 16),
        VariableToken("Pi", 18),
        OperatorToken(")", 21),
        OperatorToken("-", 23),
        NumberToken("1", 25),
        OperatorToken(")", 27)
      )))

    assert(InfixTokenizer.tokenize("-11.99/((-4.0*-Pi)-1)") ==
      Left(List(
        NumberToken("-11.99", 0),
        OperatorToken("/", 6),
        OperatorToken("(", 7),
        OperatorToken("(", 8),
        NumberToken("-4.0", 9),
        OperatorToken("*", 13),
        VariableToken("-Pi", 14),
        OperatorToken(")", 17),
        OperatorToken("-", 18),
        NumberToken("1", 19),
        OperatorToken(")", 20)
      )))

    assert(InfixTokenizer.tokenize("-11.99 / ( ( -4.0 * -3 ) - 1 )") ==
      Left(List(
        NumberToken("-11.99", 0),
        OperatorToken("/", 7),
        OperatorToken("(", 9),
        OperatorToken("(", 11),
        NumberToken("-4.0", 13),
        OperatorToken("*", 18),
        NumberToken("-3", 20),
        OperatorToken(")", 23),
        OperatorToken("-", 25),
        NumberToken("1", 27),
        OperatorToken(")", 29)
      )))

  }
  //invalid expressions
  it should "return an error when tokens are unrecognizable" in {
    assert(InfixTokenizer.tokenize("12.34.56") == Right(new ExpressionError("Failed to parse '12.34.56':0 token")))
    assert(InfixTokenizer.tokenize("12..34") == Right(new ExpressionError("Failed to parse '12.':0 token")))
    assert(InfixTokenizer.tokenize(".12") == Right(new ExpressionError("Failed to parse '.12':0 token")))
  }
}
