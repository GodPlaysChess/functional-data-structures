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

  def duplicate: ListZipper[ListZipper[A]] = {
    def until(f: ListZipper[A] => Option[ListZipper[A]],
              acc: List[ListZipper[A]] = List.empty,
              za: ListZipper[A] = this): List[ListZipper[A]] = {
      f(za) match {
        case None     => acc
        case Some(lz) => until(f, lz :: acc, lz)
      }
    }
    ListZipper[ListZipper[A]](
      until(_.moveLeft),
      this,
      until(_.moveRight) //list of all moverights until the end
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
