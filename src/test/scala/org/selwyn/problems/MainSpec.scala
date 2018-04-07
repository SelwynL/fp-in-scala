package org.selwyn.problems

import org.scalatest._

class MainSpec extends FunSuite {

  test("foo should return n squared") {
    assert(4 == Main.foo(2))
  }

  // // Define test
  // test("True should be true") {
  //   assert(true == true)
  // }
  //
  // // Ignore test
  // ignore("Ignore test") {
  //   assert(false == true)
  // }
  //
  // // Define a pending test by using (pending) for the body
  // test("Pending test") (pending)

}
