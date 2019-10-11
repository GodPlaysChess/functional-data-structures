package data

/**
  * [1, 2, 3, 4, 5, 6] is stored as [1, 2, 3] [6, 5, 4], making it faster to append and prepend,
  * at the cost of occasional rebalancing.
  * could we do better then using 2 lists beneath?
  */
case class DoubleList[A](private val front: List[A],
                         private val back: List[A]) {

  import DoubleList._

  /**
    * Unconsing through the whole list takes `O(n*log n)`
    */
  def uncons: Option[(A, DoubleList[A])] = front match {
    case x :: xs => Some(x -> DoubleList(xs, back))
    case Nil     => fromList(back).uncons
  }

  /**
    * appends element at the beginning of the list O(1)
    */
  def +>(a: A): DoubleList[A] = DoubleList(a :: front, back)

  /**
    * prepends element to the end of the list O(1)
    */
  def <+(a: A): DoubleList[A] = DoubleList(front, a :: back)

  // todo O(n + m)
  def ++(that: DoubleList[A]): DoubleList[A] = {
    fromList(toList ++ that.toList)
  }

  def map[B](f: A => B): DoubleList[B] = DoubleList(front.map(f), back.map(f))

  def join[B](implicit ev: A <:< DoubleList[B]): DoubleList[B] = {
    flatMap(implicitly[A <:< DoubleList[B]].apply)
  }

  def flatMap[B](f: A => DoubleList[B]): DoubleList[B] = {
    fromList(map(f).toList.flatMap(_.toList))
  }

  def toList: List[A] = front ++ back.reverse

  private def rebalance: DoubleList[A] = fromList(this.toList)

}

object DoubleList {
  def empty[A] = DoubleList[A](List.empty, List.empty)

  def one[A]: A => DoubleList[A] = a => DoubleList(List(a), List.empty)

  def fromList[A](list: List[A]) = {
    val (l, r) = list.splitAt(list.size / 2)
    DoubleList(l, r.reverse)
  }

  def toList[A]: DoubleList[A] => List[A] = dl => dl.front ++ dl.back.reverse

}
