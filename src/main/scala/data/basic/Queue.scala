package data.basic

import List.nil

/**
 * invariant: front is empty only if rear is empty
 * #snoc, #isEmpty takes O(1) time. #tail takes O(1) amortised time.
 */
class Queue[A](private val front: List[A], private val rear: List[A]) {
  import Queue.queue

  def isEmpty: Boolean = front.isEmpty && rear.isEmpty

  // enqueue the value
  def snoc(a: A): Queue[A] = queue(front, a :: rear)

  // dequeue the value
  def uncons: (Option[A], Queue[A]) =
    front.cata(
      None -> this,
      cons => Some(cons.head) -> queue(cons.tail, rear)
    )

  def tail: Queue[A] = uncons._2

}

object Queue {
  def empty[A] = new Queue(List.empty, List.empty)

  private[basic] def queue[A](f: List[A], r: List[A]): Queue[A] = f.cata(
    new Queue(r.reverse, List.nil),
    _ => new Queue(f, r)
  )

}
