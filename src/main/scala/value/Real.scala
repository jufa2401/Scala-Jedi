package value
import expression.Literal

case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {
  def + (other: Real) = Real(value + other.value)
  def - (other: Real) = Real(value - other.value)
  def * (other: Real) = Real(value * other.value)
  def / (other: Real): Real = {
    if (other.value == 0) throw new Exception("Division by 0")
    Real(value / other.value)
  }
  override def toString: String = value.toString
  override def hashCode: Int = toString.##


  def unary_- = Real(-value)
  def compare(other: Real): Int = {
    if (this.value < other.value) -1
    else if (other.value < value) 1
    else 0
  }
  override def canEqual(other: Any): Boolean = other.isInstanceOf[Real]
  override def equals(other: Any): Boolean =
    other match {
      case other: Real => canEqual(other) && (other.value == value)
      case _ => false
    }
}