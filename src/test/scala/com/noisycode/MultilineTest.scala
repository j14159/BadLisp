package com.noisycode

import org.scalatest._

class MultilineTest extends FunSpec with GivenWhenThen {
  describe("The SExp program parser") {
    it("should parse and evaluate multiline input") {
      given("source with two SEXPs separated by a blank line")
      val source = "(+ 2 3)\n\n(- 5 4)"

      val parseResult = SExpParser.parseProgram(source)
      parseResult match {
	case SExpParser.Success(result, _) => assert(result == List(
	  SExp(List(Id("+"), Number(2.0), Number(3.0))), 
	  SExp(List(Id("-"), Number(5.0), Number(4.0)))))
	case _ => assert(false)
      }
    }
  }

  describe("The Evaluator") {
    it("should accept parsed source and allow user-defined functions to execute") {
      val eval = new BadLispEval()

      given("predefined source with a a user function")
      val source = "(define (square x) (* x x))"
     
      and("source for a later call to that function")
      val laterCall = "(square 2)"

      then("the function source is parsed and passed into the evaluator")
      SExpParser.parseProgram(source) match {
	case SExpParser.Success(result, _) => result.map(r => eval.eval(r))
	case _ => assert(false)
      }

      when("the function call is fed in and evaluated, the result should be as expected")
      SExpParser.parseProgram(laterCall) match {
	case SExpParser.Success(result, _) => assert(eval.eval(result) == Number(4.0))
	case _ => assert(false)
      }
    }
  }
}
