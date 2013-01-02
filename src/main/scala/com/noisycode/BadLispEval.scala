package com.noisycode

class BadLispEval(initialBindings: Map[String, Term] = Map(), initialSymbols: List[PartialFunction[List[Term], Term]] = Nil)
  extends TypeHelper 
  with Bindings 
  with PredefMath 
  with Definitions 
  with Comparisons 
  with Conditionals {

  bindings = initialBindings;

  var bifs = 
    List[pf](add, sub, div, mult, constant, function, gt, lt, eq, basicIfThenElse)

  var symbolTable = initialSymbols

  def eval(s: SExp): Term = eval(s.terms)

  def eval(t: List[Term]): Term = {
    t match {
      case (Id(_) :: rest) => {
        var seed: Option[PartialFunction[List[Term], Term]] = None
	val total = symbolTable ++ bifs
	total.filter(_.isDefinedAt(t)) match {
          case List(f) => f(t)
          case (f :: more) => Error("Multiple definitions for " + f.toString + ", " + (f :: more).length)
          case _ => {
	    println("Nothing defined for:  " + t.head.toString)
	    Error("Nothing defined for:  " + t.head.toString)
	  }
        }
      }
      case (Number(n) :: Nil) => Number(n)
      case (SExp(s) :: rest) => {
        println("Evaluating SExp:  " + s.toString)
        eval(s)
      }
      case _ => Error("Bad input format:  " + t.toString)
    }
  }
}

trait TypeHelper {
  type pf = PartialFunction[List[Term], Term]
}

trait Bindings {
  var bindings: Map[String, Term] = Map()
}

trait PredefMath {
  this: TypeHelper with Bindings =>

  def eval(s: SExp): Term
  def eval(t: List[Term]): Term

  val add: pf =
    { case (Id("+") :: rest) => doMath(rest, ((a, b) => Number(a.v + b.v))) }
  val sub: pf =
    { case (Id("-") :: rest) => doMath(rest, ((a, b) => Number(a.v - b.v))) }
  val div: pf =
    { case (Id("/") :: rest) => doMath(rest, ((a, b) => Number(a.v / b.v))) }
  val mult: pf =
    { case (Id("*") :: rest) => doMath(rest, ((a, b) => Number(a.v * b.v))) }

  def doMath(t: List[Term], m: (Number, Number) => Number): Term = {
    t match {
      case (Number(v) :: rest) => rest match {
        case List() => Number(v)
        case _ => doMath(rest, m) match {
          case Number(result) => m(Number(v), Number(result))
          case error => error
        }
      }
      case (SExp(s) :: rest) => rest match {
        case List() => eval(s)
        case _ => doMath(rest, m) match {
          case Number(backResult) => eval(s) match {
            case Number(frontResult) => m(Number(frontResult), Number(backResult))
            case _ => Error("SExp did not result in a number:  " + s.toString)
          }
        }
      }
      case (Id(i) :: rest) if bindings.contains(i) => (rest, bindings(i)) match {
        case (List(), Number(n)) => Number(n)
        case (rest, Number(n)) => doMath(rest, m) match {
          case Number(result) => m(Number(n), Number(result))
          case error => error
        }
	case (List(), SExp(s)) => eval(s) match {
	  case Number(n) => Number(n)
	  case _ => Error("SExp did not result in a number:  " + s.toString)
	}
	case (rest, SExp(s)) => doMath(rest, m) match {
	  case Number(backResult) => eval(s) match {
	    case Number(frontResult) => m(Number(frontResult), Number(backResult))
	  }
	}
	case (_, somethingElse) => Error("Binding for " + i + " is not a number:  " + somethingElse.toString)
      }
    }
  }
}

