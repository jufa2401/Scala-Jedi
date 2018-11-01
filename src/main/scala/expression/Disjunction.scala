package expression

import context._
import value._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Boole = {
    if(operands.size < 2) throw new TypeException("Disjunctions need at least 2 arguments")
    val operands2 = operands.map(_.execute(env)).filter(_.isInstanceOf[Boole])

    def helper(result: Boole, unseen: List[Value]): Boole = {
      if(unseen == Nil) result else unseen.head match {
        case boole: Boole if boole.value => helper(Boole(true), Nil)
        case _ => helper(result, unseen.tail)
      }
    }
    helper(Boole(false), operands2)
  }
}