package main.scala.calculator

object TokenKind extends Enumeration {
  type TokenKind = Value
  val Number, Operator = Value
}

import TokenKind._

class Token(val text: String, val kind: TokenKind, val position: Int) {
  def canEqual(a: Any) = a.isInstanceOf[Token]

  override def equals(that: Any): Boolean =
    that match {
      case that: Token => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (text == null) 0 else text.hashCode)
    result = prime * result + kind.hashCode();
    result = prime * result + position;
    return result
  }

  override def toString(): String = s"'${text}':${position}"
}
