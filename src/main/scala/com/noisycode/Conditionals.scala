package com.noisycode

trait Conditionals {
  this: Evaluator =>

  val basicIfThenElse: pf = { 
    case List(Id("if"), cond, t, e) => { doIf(cond, t, e) } 
  }

  /**
   * Condition, Then, Else
   */
  def doIf(cond: Term, t: Term, e: Term): Term = {
    val result = cond match {
      case Bool(b) => if(b) t else e
      case Id("true") => t
      case Id("false") => e
      case SExp(s) => eval(s) match {
	case Bool(b) => if(b) t else e
	case other => Error("Condition SExp must eval to boolean:  " + s.toString)
      }
    }
    resolveTerm(result)
  }
}
