package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.{Token, TokenKind}

class TokenSpec extends FlatSpec {
  behavior of "Token equals method"

  it should "return true for same tokens" in {
    assert(new Token("12", TokenKind.Number, 0).equals(new Token("12", TokenKind.Number, 0)))
    assert(new Token("12.345", TokenKind.Number, 13).equals(new Token("12.345", TokenKind.Number, 13)))
    assert(new Token("-532", TokenKind.Number, 1).equals(new Token("-532", TokenKind.Number, 1)))
    assert(new Token("-45.00009", TokenKind.Number, 100).equals(new Token("-45.00009", TokenKind.Number, 100)))
    assert(new Token(")", TokenKind.Operator, 12).equals(new Token(")", TokenKind.Operator, 12)))
    assert(new Token("*", TokenKind.Operator, 2).equals(new Token("*", TokenKind.Operator, 2)))
  }

  it should "return false for not same tokens" in {
    assert(new Token("12", TokenKind.Number, 0).equals(new Token("-12", TokenKind.Number, 0)) == false)
    assert(new Token("12", TokenKind.Number, 0).equals(new Token("12", TokenKind.Operator, 0)) == false)
    assert(new Token("12", TokenKind.Number, 0).equals(new Token("12", TokenKind.Number, 1)) == false)
  }

  it should "return false for other types" in {
    assert(new Token("12", TokenKind.Number, 0).equals("Token") == false)
    assert(new Token("12", TokenKind.Number, 0).equals(158) == false)
    assert(new Token("12", TokenKind.Number, 0).equals(TokenKind.Number) == false)
  }
}
