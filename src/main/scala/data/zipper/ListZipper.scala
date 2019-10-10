package data.zipper

import scala.annotation.tailrec

case class ListZipper[A](left: List[A], focus: A, right: List[A]) {
  lazy val size: Int = left.size + 1 + right.size

  def moveLeft: Option[ListZipper[A]] = left match {
    case x :: xs => Option(ListZipper(xs, x, focus :: right))
    case Nil     => None
  }

  def moveRight: Option[ListZipper[A]] = right match {
    case x :: xs => Option(ListZipper(focus :: left, x, xs))
    case Nil     => None
  }

  def findLeft(p: A => Boolean): Option[ListZipper[A]] = {
    @tailrec
    def stepLeft(oz: Option[ListZipper[A]]): Option[ListZipper[A]] = oz match {
      case None                  => None
      case Some(z) if p(z.focus) => oz
      case Some(z)               => stepLeft(z.moveLeft)
    }
    stepLeft(moveLeft)
  }

  def findRight(p: A => Boolean): Option[ListZipper[A]] = {
    @tailrec
    def stepRight(oz: Option[ListZipper[A]]): Option[ListZipper[A]] = oz match {
      case None                  => None
      case Some(z) if p(z.focus) => oz
      case Some(z)               => stepRight(z.moveRight)
    }
    stepRight(moveLeft)
  }

  def modify(f: A => A): ListZipper[A] = ListZipper(left, f(focus), right)

  def delete: Option[ListZipper[A]] =
    unconsList(right).map { case (h, t)  => ListZipper(left, h, t) } orElse
      unconsList(left).map { case (h, t) => ListZipper(t, h, right) }

  def map[B](f: A => B): ListZipper[B] =
    ListZipper(left.map(f), f(focus), right.map(f))

  //  [1] 2 [3] => [[[] 1 [2, 3]], [1] 2 [3],  [[1, 2] 3 []]]
  def duplicate: ListZipper[ListZipper[A]] = {
    ListZipper(
      LazyList.unfold(this)(_.moveLeft.map(a => (a, a))).toList,
      this,
      LazyList.unfold(this)(_.moveLeft.map(a => (a, a))).toList
    )
  }

  def extract: A = focus

  private def unconsList: List[A] => Option[(A, List[A])] = {
    case Nil     => None
    case x :: xs => Option(x -> xs)
  }
}

object ListZipper {
  def fromList[A]: List[A] => Option[ListZipper[A]] = {
    case x :: xs => Option(ListZipper(List.empty, x, xs))
    case Nil     => Option.empty
  }
}
