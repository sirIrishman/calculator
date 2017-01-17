package main.scala.calculator

class Token(text: String, position: Int) {
  def Text = text

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
    result = prime * result + position;
    result = prime * result + (if (text == null) 0 else text.hashCode)
    return result
  }

  override def toString(): String = s"'${text}':${position}"
}
