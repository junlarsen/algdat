package com.supergrecko.dsa.linear

import scala.reflect.ClassTag

/** Dynamically growable array implementation with default size of 64, doubling
  * in size at increments.
  */
class DynamicTable[T : ClassTag](defaultSize: Int = 64) {
  var capacity: Int = defaultSize
  var size: Int = 0
  var data: Array[T] = new Array[T](capacity)

  /** Get an item from the array.
    *
    * This is a regular array so it can be done in constant time.
    */
  def get(index: Int): T = data(index)

  /** Insert a new value into the dynamic table.
    *
    * Its time complexity properties are as follows:
    *
    * - Best case of $$O(1)$$ when the array doesn't need to resize
    * - Average case of $$O(1)$$ since resizing should be rare
    * - Worst case of $$O(n)$$ in the case it has to resize
    */
  def push(value: T): Unit = {
    if (size >= capacity) {
      capacity *= 2
      val replacement = new Array[T](capacity * 2)
      System.arraycopy(data, 0, replacement, 0, size)
      data = replacement
    }
    data(size) = value
    size += 1
  }
}
