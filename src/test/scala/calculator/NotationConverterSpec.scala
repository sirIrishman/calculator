package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class NotationConverterSpec extends FlatSpec {
  behavior of "NotationConverter fromInfixToPostfix method"

  private def Tokenize(input: String): List[Token] = Tokenizer.tokenize(input).left.get

  // edge cases
  it should "return empty token list" in {
    val infix = Tokenize("")
    assert(NotationConverter.fromInfixToPostfix(infix) == Left(infix))
  }
  it should "return single positive integer number" in {
    val infix = Tokenize("15")
    assert(NotationConverter.fromInfixToPostfix(infix) == Left(infix))
  }
  it should "return single positive float number" in {
    val infix = Tokenize("3.12354")
    assert(NotationConverter.fromInfixToPostfix(infix) == Left(infix))
  }

  // operators
  it should "process operators with the same precedence: +, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1+2-3")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("2", 2),
        OperatorToken("+", 1),
        NumberToken("3", 4),
        OperatorToken("-", 3)
      ))
    )
  }

  it should "process operators with the same precedence: *, /" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1*4/2")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("4", 2),
        OperatorToken("*", 1),
        NumberToken("2", 4),
        OperatorToken("/", 3)
      ))
    )
  }

  it should "process operators with a different precedence: +, *" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1+3*4")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("3", 2),
        NumberToken("4", 4),
        OperatorToken("*", 3),
        OperatorToken("+", 1)
      ))
    )
  }

  it should "process operators with a different precedence: /, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("8/2-2")) ==
      Left(List[Token](
        NumberToken("8", 0),
        NumberToken("2", 2),
        OperatorToken("/", 1),
        NumberToken("2", 4),
        OperatorToken("-", 3)
      ))
    )
  }

  // parentheses + operators
  it should "process parentheses and operators with the same precedence: +, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1+(4-3)")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("4", 3),
        NumberToken("3", 5),
        OperatorToken("-", 4),
        OperatorToken("+", 1)
      ))
    )
  }

  it should "process parentheses and operators with the same precedence: *, /" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1*(4/2)")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("4", 3),
        NumberToken("2", 5),
        OperatorToken("/", 4),
        OperatorToken("*", 1)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence: +, *" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("(1+3)*4")) ==
      Left(List[Token](
        NumberToken("1", 1),
        NumberToken("3", 3),
        OperatorToken("+", 2),
        NumberToken("4", 6),
        OperatorToken("*", 5)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence : /, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("8/(4-2)")) ==
      Left(List[Token](
        NumberToken("8", 0),
        NumberToken("4", 3),
        NumberToken("2", 5),
        OperatorToken("-", 4),
        OperatorToken("/", 1)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence : /, -, *, +" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1 / 2 - 4 * -1 + 3 * -0.5")) ==
      Left(List[Token](
        NumberToken("1", 0),
        NumberToken("2", 4),
        OperatorToken("/", 2),
        NumberToken("4", 8),
        NumberToken("-1", 12),
        OperatorToken("*", 10),
        OperatorToken("-", 6),
        NumberToken("3", 17),
        NumberToken("-0.5", 21),
        OperatorToken("*", 19),
        OperatorToken("+", 15)
      ))
    )
  }

  //invalid expressions
  it should "return an error when parentheses aren't balanced" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("4-2)")) == Right(new ExpressionError("Parentheses are not balanced")))
    assert(NotationConverter.fromInfixToPostfix(Tokenize("(4-2")) == Right(new ExpressionError("Parentheses are not balanced")))
    assert(NotationConverter.fromInfixToPostfix(Tokenize("(8/(4-2)")) == Right(new ExpressionError("Parentheses are not balanced")))
    assert(NotationConverter.fromInfixToPostfix(Tokenize("8/(4-2))")) == Right(new ExpressionError("Parentheses are not balanced")))
  }

  it should "return an error when expression is invalid" in {
    assert(NotationConverter.fromInfixToPostfix(List[Token](OperatorToken("~", 2))) == Right(new ExpressionError(s"Failed to parse '~':2 token")))
  }
}
