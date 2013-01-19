package com.noisycode

trait BadStrings {
  this: Evaluator =>


  val strLength: pf = {
    case (List(Id("str-length"), t: Term)) => resolveTerm(t) match {
      case List(BadString(s)) => Number(s.length)
      case other => Error("Can't count the string length of a non-string:  " + other.toString)
    }
  }

  val str: pf = {
    case (Id("str") :: rest) => resolveTerm(eval(rest)) match {
      case l: List[Term] => collapseList(l)
      case Data(l) => collapseList(l)
      case v: Value => BadString(v)
      case other => Error(s"Could not convert $other to values")
    }
  }

  private def collapseList(l: List[Term]): Term = {
    if(l.filter(!_.isInstanceOf[Value]).length == 0) {
	val builder = new StringBuilder
	l.asInstanceOf[List[Value]].map(v => builder.append(v))
	BadString(builder.toString)
      } else
	Error("Need a list of values, not:  " + l.toString)
  }

  implicit def valueToString(v: Value): String = v match {
    case Number(n) => n.toString
    case BadChar(c) => c.toString
    case BadString(s) => s.toString
    case Bool(b) => b.toString
  }
}
