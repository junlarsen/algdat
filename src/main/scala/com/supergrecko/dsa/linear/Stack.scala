package com.supergrecko.dsa.linear

import scala.reflect.ClassTag

sealed trait StackError
case object StackOverflow extends StackError
case object StackUnderflow extends StackError

/** Stack is a linear data structure without arbitrary indexing. It is a
  * last-in-first-out data structure.
  *
  * The stack keeps an internal pointer of where the top of the stack is, making
  * its operations constant time.
  *
  * It also keeps a reference to the item after it has been popped because
  * the Stack does not ever release popped items until they've been overwritten
  * in memory position by another push.
  *
  * This is a stack implemented through an Array.
  */
class Stack[T : ClassTag](private var size: Int) {
  private val data: Array[T] = new Array[T](size)
  private var top: Int = 0

  /** Determine if the stack is empty
    *
    * This is done in constant time $$\Theta(1)$$
    */
  def empty(): Boolean = top == 0

  /** Add an item to the top of the stack
    *
    * This is done in constant time $$\Theta(1)$$
    */
  def push(value: T): Either[StackError, Unit] = {
    if (top == size - 1) {
      Left(StackOverflow)
    } else {
      top += 1
      data(top) = value

      Right(())
    }
  }

  /** Remove an item from the top of the stack
    *
    * This is done in constant time $$\Theta(1)$$
    */
  def pop(): Either[StackError, T] = {
    if (empty()) {
      Left(StackUnderflow)
    } else {
      top -= 1
      Right(data(top + 1))
    }
  }
}
