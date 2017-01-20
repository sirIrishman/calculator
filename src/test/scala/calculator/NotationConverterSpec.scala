package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator._

class NotationConverterSpec extends FlatSpec {
  behavior of "NotationConverter FromInfixToPostfix method"

  private def Tokenize(input: String): List[Token] = Tokenizer.Tokenize(input).left.get

  // edge cases
  it should "return empty token list" in {
    val infix = Tokenize("")
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(infix))
  }
  it should "return single positive integer number" in {
    val infix = Tokenize("15")
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(infix))
  }
  it should "return single positive float number" in {
    val infix = Tokenize("3.12354")
    assert(NotationConverter.FromInfixToPostfix(infix) == Left(infix))
  }

  // operators
  it should "process operators with the same precedence: +, -" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("1+2-3")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 0),
        new Token("2", TokenType.Number, 2),
        new Token("+", TokenType.Operator, 1),
        new Token("3", TokenType.Number, 4),
        new Token("-", TokenType.Operator, 3)
      ))
    )
  }

  it should "process operators with the same precedence: *, /" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("1*4/2")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 0),
        new Token("4", TokenType.Number, 2),
        new Token("*", TokenType.Operator, 1),
        new Token("2", TokenType.Number, 4),
        new Token("/", TokenType.Operator, 3)
      ))
    )
  }

  it should "process operators with a different precedence: +, *" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("1+3*4")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 0),
        new Token("3", TokenType.Number, 2),
        new Token("4", TokenType.Number, 4),
        new Token("*", TokenType.Operator, 3),
        new Token("+", TokenType.Operator, 1)
      ))
    )
  }

  it should "process operators with a different precedence: /, -" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("8/2-2")) ==
      Left(List[Token](
        new Token("8", TokenType.Number, 0),
        new Token("2", TokenType.Number, 2),
        new Token("/", TokenType.Operator, 1),
        new Token("2", TokenType.Number, 4),
        new Token("-", TokenType.Operator, 3)
      ))
    )
  }

  // parentheses + operators
  it should "process parentheses and operators with the same precedence: +, -" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("1+(4-3)")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 0),
        new Token("4", TokenType.Number, 3),
        new Token("3", TokenType.Number, 5),
        new Token("-", TokenType.Operator, 4),
        new Token("+", TokenType.Operator, 1)
      ))
    )
  }

  it should "process parentheses and operators with the same precedence: *, /" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("1*(4/2)")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 0),
        new Token("4", TokenType.Number, 3),
        new Token("2", TokenType.Number, 5),
        new Token("/", TokenType.Operator, 4),
        new Token("*", TokenType.Operator, 1)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence: +, *" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("(1+3)*4")) ==
      Left(List[Token](
        new Token("1", TokenType.Number, 1),
        new Token("3", TokenType.Number, 3),
        new Token("+", TokenType.Operator, 2),
        new Token("4", TokenType.Number, 6),
        new Token("*", TokenType.Operator, 5)
      ))
    )
  }

  it should "process parentheses and operators with a different precedence : /, -" in {
    assert(NotationConverter.FromInfixToPostfix(Tokenize("8/(4-2)")) ==
      Left(List[Token](
        new Token("8", TokenType.Number, 0),
        new Token("4", TokenType.Number, 3),
        new Token("2", TokenType.Number, 5),
        new Token("-", TokenType.Operator, 4),
        new Token("/", TokenType.Operator, 1)
      ))
    )
  }
}
