package stack

import linkedlist.LinkedList

trait StackProtocol[A] {
  def push(elem: A): Unit
  def pop(): A
  def top: A
}

// must be "invariant" because it's mutable
final class Stack[A] private(elems: A*) extends StackProtocol[A] {

  private var elements: LinkedList[A] = LinkedList[A](elems: _*)

  override def push(elem: A): Unit = elements = elements.append(elem)

  override def pop(): A = {
    val popped = elements.head
    elements = elements.tail

    popped
  }

  override def top: A = elements.head
}

object Stack {
  def apply[A](elements: A*): Stack[A] = new Stack(elements: _*)
}
