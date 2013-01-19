package com.noisycode

/**
 * Provides support for using "define" to create global values and functions.
 */
trait Definitions {
  this: Evaluator =>

  val constant: pf =
    {
      case List(Id("define"), Id(id), Number(value)) => {
        bindings = bindings + (id -> Number(value))
        Sym(id, Number(value))
      }
      case List(Id("define"), Id(id), other) => {
        resolveTerm(other) match {
          case v: Value => {
            bindings = bindings + (id -> v)
            Sym(id, v)
          }
	  case Data(d) => {
	    bindings = bindings + (id -> Data(d))
            Sym(id, Data(d))
	  }
          case _ => Error("SExp in define of non-function must result in a concrete value:  " + other.toString)
        }
      }
    }
  val function: pf =
    {
      case List(Id("define"), SExp(params: List[Id]), SExp(body)) => {
        val funcName = params.head
        val parameters = params.tail.map(_.id)

        val thisFunc: pf = {
          case (Id(fn) :: rest) if rest.length == parameters.length && fn == funcName.id => {
	    val resolvedArgs = rest.map {
	      p => resolveTerm(p)
	    }

	    val totalBindings = parameters.zip(resolvedArgs).toMap ++ bindings
            new BadLispEval(totalBindings, symbolTable).eval(body)
          }
        }
        symbolTable = thisFunc :: symbolTable

        Func(params.head.id, params.tail, SExp(body))
      }
    }
}
