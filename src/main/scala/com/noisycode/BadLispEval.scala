package com.noisycode

trait Evaluator {
  type pf = PartialFunction[List[Term], Term]
  
  var symbolTable: List[PartialFunction[List[Term], Term]] = Nil

  def eval(t: List[Term]): Term

  def eval(lt: ListTerm): Term = lt match {
    case SExp(s) => eval(s)
    case Data(d) => Data(d)
  }

  def resolveTerm(t: Term): Value = {
    t match {
      case v: Value => v
      case Id(id) => {
        var seed: Option[PartialFunction[List[Term], Term]] = None
	val total = symbolTable
	total.filter(_.isDefinedAt(List(t))) match {
	  /*
	   * In theory, the following could allow for cyclical definitions
	   * but I'm not especially concerned about fixing that at present.
	   */
          case (f :: more) => resolveTerm(f(List(t)))  
          case _ => Error(s"Nothing defined for $t")
	}
      }
      case SExp(s) => eval(s) match {
	case v: Value => v
	case other => Error("Could not resolve SExp to number:  " + s.toString)
      }
      case Data(d) => Data(d)
    }
  }
}

/**
 * Full BadLisp interpreter.  When evaluating a function, the interpreter will spawn new instances to give
 * locally scoped function parameter bindings.
 */
class BadLispEval(initialSymbols: List[PartialFunction[List[Term], Term]] = Nil)
  extends Evaluator
  with PredefMath 
  with Definitions 
  with Comparisons 
  with Conditionals 
  with BadLists {

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
          case (f :: more) => f(t)  //always execute the most recently defined function.
          case _ => {
	    println("Nothing defined for:  " + t.head.toString)
	    Error("Nothing defined for:  " + t.head.toString)
	  }
        }
      }
      case List(v: Value) => v
      case (SExp(s) :: rest) => eval(s)
      case _ => Error("Bad input format:  " + t.toString)
    }
  }
}

/**
 * Predefined math functions.
 */
trait PredefMath {
  this: Evaluator =>

  val add: pf =
    { case (Id("+") :: rest) => doMath(rest, ((a, b) => Number(a.n + b.n))) }
  val sub: pf =
    { case (Id("-") :: rest) => doMath(rest, ((a, b) => Number(a.n - b.n))) }
  val div: pf =
    { case (Id("/") :: rest) => doMath(rest, ((a, b) => Number(a.n / b.n))) }
  val mult: pf =
    { case (Id("*") :: rest) => doMath(rest, ((a, b) => Number(a.n * b.n))) }

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
