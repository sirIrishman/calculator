package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.{NotationConverter, Token, TokenType, Tokenizer}

class NotationConverterSpec extends FlatSpec {
  behavior of "NotationConverter FromInfixToPostfix method"

  // operators
  it should "process operators with the same precedence: +, -" in {
    val infix = Tokenizer.Tokenize("1+2-3").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("1*4/2").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("1+3*4").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("8/2-2").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("1+(4-3)").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("1*(4/2)").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("(1+3)*4").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
    val infix = Tokenizer.Tokenize("8/(4-2)").left.get
    assert(NotationConverter.FromInfixToPostfix(infix) ==
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
