package com.noisycode

/**
 * Provides support for using "define" to create global values and functions.
 */
trait Definitions {
  this: Evaluator =>

  val constant: pf =
    {
      case List(Id("define"), Id(id), other) => {
        resolveTerm(other) match {
          case v: Value => {
            val resolverFunc: pf = {
              case List(Id(i)) if i == id => v
            }

            symbolTable = resolverFunc :: symbolTable
            Sym(id, v)
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
            /*
	     * This is kludgy but works, doesn't get stack overflows
	     * on basic recursive functions like the old "new evaluator"
	     * approach did.  Probably better to have a definition of eval()
	     * that takes symbol table arguments or something like that.
	     */
            val old = symbolTable

            val resolvedArgs = rest.map(resolveTerm)
	    val paramsAndArgs = parameters.zip(resolvedArgs)

            val localBindings: List[pf] = paramsAndArgs.map {
              tup =>
                val resolverFunc: pf = {
                  case List(Id(tup._1)) => tup._2
                }
		resolverFunc
            }

	    paramsAndArgs.map {
	      _ match {
		case (binding, Func(originalName, p, b)) => eval(SExp(List(Id("define"), SExp(Id(binding) :: p), b)))
		case _ => {}
	      }
	    }

            symbolTable = localBindings ::: symbolTable
            val result = eval(body)
            symbolTable = old
            result
          }
        }
	val referenceFunc: pf = {
	  case List(Id(fn)) if fn == params.head.id => Func(params.head.id, params.tail, SExp(body))
	}

        symbolTable = thisFunc :: referenceFunc :: symbolTable
        Func(params.head.id, params.tail, SExp(body))
      }
    }
}
