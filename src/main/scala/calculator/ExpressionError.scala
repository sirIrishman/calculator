package main.scala.calculator

class ExpressionError(val message: String) {
  def canEqual(a: Any) = a.isInstanceOf[ExpressionError]

  override def equals(that: Any): Boolean =
    that match {
      case that: ExpressionError => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }

  override def hashCode: Int = {
    val prime = 31
    var result = 1
    result = prime * result + (if (message == null) 0 else message.hashCode)
    return result
  }

  override def toString(): String = message
}
