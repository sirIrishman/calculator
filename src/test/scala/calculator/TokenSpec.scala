package test.scala.calculator

import org.scalatest.FlatSpec
import main.scala.calculator.{Token, TokenType}

class TokenSpec extends FlatSpec {
  behavior of "Token equals method"

  it should "return true for same tokens" in {
    assert(new Token("12", TokenType.Number, 0).equals(new Token("12", TokenType.Number, 0)))
    assert(new Token("12.345", TokenType.Number, 13).equals(new Token("12.345", TokenType.Number, 13)))
    assert(new Token("-532", TokenType.Number, 1).equals(new Token("-532", TokenType.Number, 1)))
    assert(new Token("-45.00009", TokenType.Number, 100).equals(new Token("-45.00009", TokenType.Number, 100)))
    assert(new Token(")", TokenType.Operator, 12).equals(new Token(")", TokenType.Operator, 12)))
    assert(new Token("*", TokenType.Operator, 2).equals(new Token("*", TokenType.Operator, 2)))
  }

  it should "return false for not same tokens" in {
    assert(new Token("12", TokenType.Number, 0).equals(new Token("-12", TokenType.Number, 0)) == false)
    assert(new Token("12", TokenType.Number, 0).equals(new Token("12", TokenType.Operator, 0)) == false)
    assert(new Token("12", TokenType.Number, 0).equals(new Token("12", TokenType.Number, 1)) == false)
  }
}
