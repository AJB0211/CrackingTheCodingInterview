package CrackingTheCodingInterview

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest.prop.Checkers
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._

import org.scalatest.exceptions.TestFailedException

class UniqueTest extends FunSuite {
  test("abcd") {
    assert(UniqueChars.isUnique("abcd") === true)
  }

  test("aacd") {
    assert(UniqueChars.isUnique("aacd") === false)
  }

}