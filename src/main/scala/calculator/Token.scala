package main.scala.calculator

import main.scala.calculator.TokenType._

class Token(text: String, `type`: TokenType, position: Int) {
  def Text = text

  def Type = `type`

  def Position = position

  def canEqual(a: Any) = a.isInstanceOf[Token]

  override def equals(that: Any): Boolean =
    that match {
      case that: Token => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (Text == null) 0 else Text.hashCode)
    result = prime * result + Type.hashCode();
    result = prime * result + Position;
    return result
  }

  override def toString(): String = s"'${text}':${position}"
}
