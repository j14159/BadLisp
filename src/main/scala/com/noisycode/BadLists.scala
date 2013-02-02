package com.noisycode

trait BadLists {
  this: Evaluator =>

  val cons: pf =
    {
      case List(Id("cons"), t: Term, backend) => resolveTerm(backend) match {
        case n: Number => Data(List(resolveTerm(t), n))
        case Data(d) => Data(resolveTerm(t) :: d)
	case BadString(s) => resolveTerm(t) match {
	  case BadChar(c) => BadString(c + s)
	  case BadString(s2) => BadString(s2 + s)
	  case Data(d) => Data(d ::: List(BadString(s)))
	  case _ => Data(List(t, BadString(s)))
	}
	case BadChar(c) => resolveTerm(t) match {
	  case BadChar(c2) => BadString(c2 + c.toString)
	  case BadString(s2) => BadString(s2 + c.toString)
	  case Data(d) => Data(d ::: List(BadChar(c)))
	  case _ => Data(List(t, BadChar(c)))
	}
        case other => Error("Not something you can cons onto:  " + other.toString)
      }
    }
  val car: pf =
    {
      case List(Id("car"), check) => resolveTerm(check) match {
        case Data(d) => resolveTerm(d.head)
	case BadString(s) => BadChar(s.head)
        case other => Error("car needs a list, not " + other.toString)
      }
    }
  val cdr: pf =
    {
      case List(Id("cdr"), check) => resolveTerm(check) match {
        case Data(d) => Data(d.tail)
	case BadString(s) => BadString(s.tail)
        case other => Error("cdr needs a list, not " + other.toString)
      }
    }
}
