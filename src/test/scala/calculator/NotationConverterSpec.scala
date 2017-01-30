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
        new Token("1", TokenKind.Number, 0),
        new Token("2", TokenKind.Number, 2),
        new Token("+", TokenKind.Operator, 1),
        new Token("3", TokenKind.Number, 4),
        new Token("-", TokenKind.Operator, 3)
      ))
    )
  }

  it should "process operators with the same precedence: *, /" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1*4/2")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 0),
        new Token("4", TokenKind.Number, 2),
        new Token("*", TokenKind.Operator, 1),
        new Token("2", TokenKind.Number, 4),
        new Token("/", TokenKind.Operator, 3)
      ))
    )
  }

  it should "process operators with a different precedence: +, *" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1+3*4")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 0),
        new Token("3", TokenKind.Number, 2),
        new Token("4", TokenKind.Number, 4),
        new Token("*", TokenKind.Operator, 3),
        new Token("+", TokenKind.Operator, 1)
      ))
    )
  }

  it should "process operators with a different precedence: /, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("8/2-2")) ==
      Left(List[Token](
        new Token("8", TokenKind.Number, 0),
        new Token("2", TokenKind.Number, 2),
        new Token("/", TokenKind.Operator, 1),
        new Token("2", TokenKind.Number, 4),
        new Token("-", TokenKind.Operator, 3)
      ))
    )
  }

  // parentheses + operators
  it should "process parentheses and operators with the same precedence: +, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1+(4-3)")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 0),
        new Token("4", TokenKind.Number, 3),
        new Token("3", TokenKind.Number, 5),
        new Token("-", TokenKind.Operator, 4),
        new Token("+", TokenKind.Operator, 1)
      ))
    )
  }

  it should "process parentheses and operators with the same precedence: *, /" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1*(4/2)")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 0),
        new Token("4", TokenKind.Number, 3),
        new Token("2", TokenKind.Number, 5),
        new Token("/", TokenKind.Operator, 4),
        new Token("*", TokenKind.Operator, 1)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence: +, *" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("(1+3)*4")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 1),
        new Token("3", TokenKind.Number, 3),
        new Token("+", TokenKind.Operator, 2),
        new Token("4", TokenKind.Number, 6),
        new Token("*", TokenKind.Operator, 5)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence : /, -" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("8/(4-2)")) ==
      Left(List[Token](
        new Token("8", TokenKind.Number, 0),
        new Token("4", TokenKind.Number, 3),
        new Token("2", TokenKind.Number, 5),
        new Token("-", TokenKind.Operator, 4),
        new Token("/", TokenKind.Operator, 1)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence : /, -, *, +" in {
    assert(NotationConverter.fromInfixToPostfix(Tokenize("1 / 2 - 4 * -1 + 3 * -0.5")) ==
      Left(List[Token](
        new Token("1", TokenKind.Number, 0),
        new Token("2", TokenKind.Number, 4),
        new Token("/", TokenKind.Operator, 2),
        new Token("4", TokenKind.Number, 8),
        new Token("-1", TokenKind.Number, 12),
        new Token("*", TokenKind.Operator, 10),
        new Token("-", TokenKind.Operator, 6),
        new Token("3", TokenKind.Number, 17),
        new Token("-0.5", TokenKind.Number, 21),
        new Token("*", TokenKind.Operator, 19),
        new Token("+", TokenKind.Operator, 15)
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
    assert(NotationConverter.fromInfixToPostfix(List[Token](new Token("~", TokenKind.Operator, 2))) == Right(new ExpressionError(s"Failed to parse '~':2 token")))
  }
}
