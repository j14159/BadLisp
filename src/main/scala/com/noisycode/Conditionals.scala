package com.noisycode

trait Conditionals {
  this: TypeHelper with Bindings =>

  def eval(s: SExp): Term
  def eval(t: List[Term]): Term

  val basicIfThenElse: pf = { 
    case List(Id("if"), cond, t, e) => { doIf(cond, t, e) } 
  }

  /**
   * Condition, Then, Else
   */
  def doIf(cond: Term, t: Term, e: Term): Term = {
    cond match {
      case Bool(b) => if(b) t else e
      case Id("true") => t
      case Id("false") => e
      case SExp(s) => eval(s) match {
	case Bool(b) => if(b) t else e
	case other => Error("Condition SExp must eval to boolean:  " + s.toString)
      }
    }
  }
}
