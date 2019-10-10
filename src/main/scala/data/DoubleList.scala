package data

/*
   [1, 2, 3, 4, 5, 6] is stored as [1, 2, 3] [6, 5, 4], making it faster to append and prepend,
    at the cost of occasional rebalancing.
 */
case class DoubleList[A](private val back: List[A],
                         private val forward: List[A]) {
  import DoubleList._

  /**
    * Todo: uncons the whole list takes `O(n^2)` instead of O(n)
    */
  def uncons[A]: Option[(A, DoubleList[A])] = back match {
    case x :: xs => Some(x -> DoubleList(xs, forward))
    case Nil     => forward.headOption.map(_ -> fromList(forward.tail))
  }

  /**
    * appends element at the beginning of the list O(1)
    */
  def +>(a: A): DoubleList[A] = DoubleList(a :: back, forward)

  /**
    * prepends element to the end of the list O(1)
    */
  def <+(a: A): DoubleList[A] = DoubleList(back, a :: forward)

  def map[B](f: A => B): DoubleList[B] = DoubleList(back.map(f), forward.map(f))

//  def flatMap[B](f: A => DoubleList[B]): DoubleList[B] = {
//    val fst = back.map(f)
//    val fwd = forward.map(f)
//  }
}

object DoubleList {
  def empty[A] = DoubleList[A](List.empty, List.empty)

  def one[A]: A => DoubleList[A] = a => DoubleList(List(a), List.empty)

  def fromList[A](list: List[A]) = {
    val (l, r) = list.splitAt(list.size / 2)
    DoubleList(l, r.reverse)
  }

  def toList[A]: DoubleList[A] => List[A] = dl => dl.back ++ dl.forward.reverse
}
