import expression._
import value._
import context.Environment

object ExpTest extends App {

  val globalEnv = new Environment
  val pi = Identifier("pi")
  val num = Real(3.14)
  globalEnv.put(pi, num)
  println(pi.execute(globalEnv))
  println(num.execute(globalEnv))
}