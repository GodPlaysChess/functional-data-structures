package data.basic

import List.nil

/**
 * invariant: front is empty only if rear is empty
 */
class Queue[A](private val front: List[A], private val rear: List[A]) {
  def isEmpty: Boolean = front.isEmpty && rear.isEmpty

  def snoc(a: A): Queue[A] = front.cata(new Queue(List(a), List.nil), _  => new Queue(front, a :: rear))

  def uncons: (Option[A], Queue[A]) =
    front.cata(
      None -> this,
      cons => (Some(cons.head) -> cons.tail.cata(
        new Queue(rear.reverse, nil),
        xss => new Queue(xss, rear)))
    )

  def tail: Queue[A] = uncons._2
}

object Queue {
  def empty[A] = new Queue(List.empty, List.empty)

}
