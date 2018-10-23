package value
import expression.Literal

case class Boole(val value: Boolean) extends Literal {
  override def toString: String = this.value.toString
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)
}