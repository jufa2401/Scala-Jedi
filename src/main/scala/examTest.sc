
def applyFilters[T](listType: List[T], tests: List[T => Boolean]): List[T] = {
  var list = listType
  tests.foreach(test => list = list.filter(test))
  list
}

val myTests = List((x: Int) => x % 2 == 0, (x: Int) => x > 10, (x: Int) => x < 50) // my tests
applyFilters(List(7, 8, 12, 15, 20, 52), myTests)

def addPositives(list: List[Option[Int]]): Int = {
  def isPositive(x: Option[Int]): Boolean = {
    x match {
      case Some(num) => num > 0
      case None => false
    }
  }
  def mapToInts(x: Option[Int]): Int = {
    x match {
      case Some(num) => num
      case None => 0
    }
  }
  list.filter(isPositive).map(mapToInts).sum
}

addPositives(List(Some(3), None, Some(4), Some(-5)))

//def compose[X](f:X=>X, g:X=>X):X=> X=(x:X) => f(g(x))

def listCompose[T](funs: List[T=>T]): T=>T = {
  funs.reduce((function,function2) => function.compose(function2))
}
def square(x:Int) = x*x
def triple(x:Int) = 3*x

def func = listCompose(List(square _,triple _))
func(2)

// # i with f(inputs(i)) != outputs(i)
def unitTest[S, T](inputs: List[S], outputs: List[T], f: S=>T): Int = {
  inputs.map(f).zip(outputs).count(tuple => tuple._1 != tuple._2)
}
unitTest(List(1, 2, 3), List(1, 4, 8), (x: Int) => x * x)


//def g = iter((t: Int) => 2*t, 3)
//g(5)

def ackermann(m: Int, n: Int): Int =
if (m == 0) n + 1
else if (n == 0) ackermann(m - 1, 1)
else ackermann(m - 1, ackermann(m, n - 1))

def fib_tailrec(n: Int) = {
  def helper(n: Int, a: Int, b: Int): Int =
  if (n == 0) a
  else if (n == 1) b
  else helper(n - 1, b, a + b)

  helper(n, 1, 1)
}

//def iter(f: Int=>Int, n: Int): Int = {
//  def helper(result: Int, count: Int): Int = {
//    if(count > n) result
//    else helper(result + f(f),count+1)
//  }
//  helper(0,0)
//}
/*
def iter = lambda(f,n) { def helper = lambda(result, count) { if (count>n) result else helper( result+f(f), count+1)} ; helper(0,0)}
 */

case class Entry(val hour: Int, val temp: Double) {
  override def toString = "[" + hour + ":00 temp = " + temp + " degs]"
}
val readings = List(Entry(6, 25), Entry(10, 28), Entry(12, 32), Entry(16, 30), Entry(18, 26), Entry(22, 19))

def logProcessor(log: List[Entry]) = {
  def celsius2fahrenheit(celsius: Double): Double = 1.8 * celsius + 32
  log.filter(_.hour < 17).map(entry => Entry(entry.hour,celsius2fahrenheit(entry.temp)))
}
logProcessor(readings)

def foo(x: Int) = {
  x*x-7*x+10
}

def countRoots(number:Int, f: Int=> Int) = {
  var count = 0
  var n = 0
  while(n<= number) {
    println(f(number))
    if(f(number) == 0) count = count+1
    n = n+1
  }
  count

}
countRoots(10,foo)

