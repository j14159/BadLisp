package com.noisycode

import org.scalatest._

class ParensBalancerTest extends FunSpec with GivenWhenThen {
  describe("The Repl balancedParens method") {
    it("should correctly find unbalanced and balanced strings") {
      given("a Repl instance")
      val repl = new Repl()

      when("an unblanced string is checked, balancedParens should be false")
      assert(repl.balancedParens("(define (square x)") == false)

      when("a balanced string is checked, balancedParens should be true")
      assert(repl.balancedParens("(define (square x) (* x x))") == true)

      when("a messy unbalanced string of nonsense is checked, it should be false")
      assert(repl.balancedParens("(d ()()(()))(x 2(()") == false)
    }
  }
}
