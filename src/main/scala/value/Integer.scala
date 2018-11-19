package value
import expression.Literal
// Pretty much the same class as real, but with Ints instead of doubles, and made to a case class instead of companion object
case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals {
  def + (other: Integer): Integer = Integer(value + other.value)
  def * (other: Integer): Integer = Integer(value * other.value)
  def - (other: Integer): Integer = Integer(value - other.value)
  def / (other: Integer): Integer = {
    if (other.value == 0) throw new Exception("Division by 0")
    Integer(value / other.value)
  }
  override def toString: String = value.toString

  def unary_- = Integer(-value)
  def compare(other: Integer): Int = {
    if (value < other.value) -1
    else if (other.value < value) 1
    else 0
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean =
    other match {
      case other: Integer => canEqual(other) && (other.value == value)
      case _ => false
    }

  override def hashCode: Int = toString.##
}

object Integer {
  import scala.language.implicitConversions
  implicit def intToReal(n: Integer): Real = Real(n.value.toDouble)
}