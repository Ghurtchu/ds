package queue

import linkedlist.LinkedList

trait QueueProtocol[A] {
  def enqueue(elem: A): Unit
  def dequeue: A
}

final class Queue[A](elems: A*) extends QueueProtocol[A] {

  private var elements: LinkedList[A] = LinkedList[A](elems: _*)

  override def enqueue(elem: A): Unit = elements = LinkedList.nonEmpty(elem, elements)

  override def dequeue: A = {
    val dequeued = elements.last
    val newElements = elements.dropLast
    elements = newElements

    dequeued
  }

  override def toString: String = elements.toString.replace("LinkedList(", "Queue(")

}

object Queue {
  def apply[A](elems: A*): Queue[A] = new Queue(elems: _*)
}
