package com.noisycode

trait BadStrings {
  this: Evaluator =>


  val strLength: pf = {
    case (List(Id("str-length"), t: Term)) => resolveTerm(t) match {
      case BadString(s) => Number(s.length)
      case other => Error("Can't count the string length of a non-string:  " + other.toString)
    }
  }

  val str: pf = {
    case (Id("str") :: rest) => {
      val s = rest.foldLeft(new StringBuilder)((b, n) => n match {
	case v: Value => b.append(valueToString(v))
	case other => b.append(valueToString(resolveTerm(other)))
      }).toString
      BadString(s)
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
