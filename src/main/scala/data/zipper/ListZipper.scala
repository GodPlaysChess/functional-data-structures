package data.zipper

import control.Comonad
import data.Stream
import Stream.unfold

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
    stepRight(moveRight)
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
      Stream.unfold(this)(_.moveLeft.map(a => (a, a))).toList,
      this,
      Stream.unfold(this)(_.moveLeft.map(a => (a, a))).toList
    )
  }

  private def unconsList: List[A] => Option[(A, List[A])] = {
    case Nil     => None
    case x :: xs => Option(x -> xs)
  }

  def any(p: A => Boolean): Boolean =
    p(focus) || left.exists(p) || right.exists(p)

  def all(p: A => Boolean): Boolean =
    p(focus) && left.exists(p) && right.exists(p)

  def find(p: A => Boolean): Option[ListZipper[A]] =
    Some(this).filter(_ => p(focus)) orElse findLeft(p) orElse findRight(p)
  
  def toList: List[A] = left ++ (focus :: right)

  def allWithAdjacent(a: A): List[ListZipper[A]] = {
    duplicate.toList.filter(_.adjacentFociiSatisfy(_ == a))
  }

  // Do any adjacent focii of the list zipper satisfy the given predicate
  private def adjacentFociiSatisfy(p: A => Boolean): Boolean = {
    val cmp: Option[ListZipper[A]] => Boolean = _.exists(l => p(l.focus))
    cmp(moveLeft) || cmp(moveRight)
  }

  override def toString: String = {
    s"${left.mkString("|", " ", "\n")} ($focus)\n ${right.mkString("|", " ", "\n")}"
  }
}

object ListZipper {
  def fromList[A]: List[A] => Option[ListZipper[A]] = {
    case x :: xs => Option(ListZipper(List.empty, x, xs))
    case Nil     => Option.empty
  }

  implicit val Instance: Comonad[ListZipper] = new Comonad[ListZipper] {
    override def extract[A](fa: ListZipper[A]) = fa.focus

    override def duplicate[A](f: ListZipper[A]) = f.duplicate

    override def map[A, B](fa: ListZipper[A])(f: A => B) = fa.map(f)
  }
}
