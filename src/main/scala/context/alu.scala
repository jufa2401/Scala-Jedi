package context

import expression._
import value._

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {

  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) //binary
      case "more" => more(args) // binary
      case "equals" => equals(args) // note: equals(7, true) = false, not error
      case "unequals" => unequals(args) // binary, = not(equals(args))?
      case "not" => not(args) // unary
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  private def dereference(args: List[Value]): Value = args match {
//TODO:    Wow, some is useful, lol
    case List(a) =>
      Some(a)
        .filter(_.isInstanceOf[Variable])
        .map(_.asInstanceOf[Variable])
        .map(_.content)
        .getOrElse(throw new TypeException("Expected a variable"))
    case _ => throw new TypeException("Cannot dereference variable: One input expected")
  }

  private def makeVar(args: List[Value]): Value = {
//    args.size == 1
//    new Variable(args(0))
    if(args.size == 1) new Variable(args.head)
    else throw new TypeException("Variables can only have 1 argument")
  }

  private def toInt(arg: Value): Option[Integer] =
    if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None

  private def toReal(arg: Value): Option[Real] =
    if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real])
    else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
    else None

  private def toChars(arg: Value): Option[Chars] =
    if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None

  private def toBoole(arg: Value): Option[Boole] =
    if(arg.isInstanceOf[Boole]) Some(arg.asInstanceOf[Boole]) else None

  private def add(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) args2.flatten.reduce(_+_) // weird highlighting error in IntelliJ
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) args3.flatten.reduce(_+_)
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) args4.flatten.reduce(_+_)
        else {
          throw new TypeException("Inputs to + must be numbers or texts")
        }
      }
    }
  }

  private def mul(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if(args2.size == args.size) args2.flatten.reduce(_*_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if(args3.size == args.size) args3.flatten.reduce(_*_)
      else {
        throw new TypeException("Inputs to * must be numbers")
      }
    }
  }

  private def sub(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if(args2.size == args.size) args2.flatten.reduce(_-_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if(args3.size == args.size) args3.flatten.reduce(_-_)
      else {
        throw new TypeException("Inputs to - must be numbers")
      }
    }
  }

  private def div(args: List[Value]) = {
    val args2 = args.map(toInt).filter(_ != None)
    if(args2.size == args.size) args2.flatten.reduce(_/_)
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if(args3.size == args.size) args3.flatten.reduce(_/_)
      else {
        throw new TypeException("Inputs to / must be numbers")
      }
    }
  }

  private def less(args: List[Value]): Value = {
    if (args.length  != 2) throw new TypeException("less expects two inputs")
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) Boole(args2(0) < args2(1))
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) Boole(args3(0) < args3(1))
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) Boole(args4(0).get < args4(1).get)
        else throw new TypeException("Inputs to < must be numbers or texts")
      }
    }
  }

  private def more(args: List[Value]) = {
    if (args.length != 2) throw new TypeException("more expects two inputs")
    val args2 = args.map(toInt).filter(_ != None)
    if (args2.size == args.size) Boole(args2.head > args2(1))
    else {
      val args3 = args.map(toReal).filter(_ != None)
      if (args3.size == args.size) Boole(args3.head > args3(1))
      else {
        val args4 = args.map(toChars).filter(_ != None)
        if (args4.size == args.size) Boole(args4.head > args4(1))
        else throw new TypeException("Inputs to < must be numbers")
      }
    }
  }

  private def equals(args: List[Value]): Value = {
    if (args.length < 2) throw new TypeException("equals expects two or more inputs")
    else {
      val a = args.head
      for (e <- args) {
        if (a != e) return Boole(false)
      }
      Boole(true)
    }
  }

  private  def unequals(args: List[Value]) = {
    not(List(equals(args)))
  }

  private def not(args: List[Value]) = {
    if(args.length != 1) throw new TypeException("not expects only 1 argument")
    val args2 = args.map(toBoole).filter(_ != None)
    if(args2.size == args.size) !args2(0).get
    else {
      throw new TypeException("not expects number or boole")
    }
  }

  def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
  def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
  def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }


  // etc.
}