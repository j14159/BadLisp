package com.noisycode

trait Evaluator {
  type pf = PartialFunction[List[Term], Term]
  
  var bindings: Map[String, Term] = Map()
  var symbolTable: List[PartialFunction[List[Term], Term]] = Nil

  def eval(t: List[Term]): Term

  def eval(lt: ListTerm): Term = lt match {
    case SExp(s) => eval(s)
    case Data(d) => Data(d)
  }

  def resolveTerm(t: Term): Value = {
    t match {
      case v: Value => v
      case Id(id) => bindings(id) match {
	case Number(n) => Number(n)
	case Data(d) => Data(d)
	case other => Error("Not a value binding:  " + other.toString)
      }
      case SExp(s) => eval(s) match {
	case v: Value => v
	case other => Error("Could not resolve SExp to number:  " + s.toString)
      }
      case Data(d) => Data(d)
    }
  }
}

class BadLispEval(initialBindings: Map[String, Term] = Map(), initialSymbols: List[PartialFunction[List[Term], Term]] = Nil)
  extends Evaluator
  with PredefMath 
  with Definitions 
  with Comparisons 
  with Conditionals 
  with BadLists {

  bindings = initialBindings
  symbolTable = initialSymbols

  var bifs = 
    List[pf](add, sub, div, mult, 
	     constant, function, 
	     gt, lt, eq, 
	     basicIfThenElse,
	     cons, car, cdr)

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

trait PredefMath {
  this: Evaluator =>

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
      case List(Number(n)) => Number(n)
      case (Number(n) :: rest) => doMath(rest, m) match {
	case Number(x) => m(Number(n), Number(x))
	case error => error
      }
      case List(other) => resolveTerm(other)
      case (other :: rest) => (resolveTerm(other), doMath(rest, m)) match {
	case (Number(x), Number(y)) => m(Number(x), Number(y))
	case wrong => Error("One or both not resolving to numbers:  " + t.toString)
      }
    }
  }
}

