package com.supergrecko.dsa.linear

import org.scalatest.funsuite.AnyFunSuite

class StackTest extends AnyFunSuite {
  test("it can push and pop items") {
    val stack = new Stack[Int](10)
    assert(stack.empty())
    val maybeErr = stack.push(10)
    assert(maybeErr.contains(()))
    assert(!stack.empty())
    val ten = stack.pop()
    assert(stack.empty())
    assert(ten.contains(10))
  }

  test("it will overflow at max items") {
    val stack = new Stack[Int](3)
    stack.push(1)
    stack.push(1)
    stack.push(1)
    val overflow = stack.push(10)
    assert(overflow.swap.contains(StackOverflow))
  }

  test("it will underflow at zero items") {
    val stack = new Stack[Int](10)
    val underflow = stack.pop()
    assert(underflow.swap.contains(StackUnderflow))
  }
}
