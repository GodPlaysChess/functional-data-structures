package data.basic

import scala.annotation.tailrec

sealed abstract class List[A] {
  def ++(list: List[A]): List[A] = {
    def go(dec: List[A], res: List[A]): List[A] = dec match {
      case h Cons t => go(t, h :: res)
      case _        => res
    }
    go(reverse, list)
  }

  def ::(head: A): List[A] = Cons(head, this)

  def reverse: List[A] = List.reverse(this)

}
case class Cons[A](head: A, tail: List[A]) extends List[A]
case object X                              extends List[A forSome { type A }]

object List extends ListInstances {

  def fromScala[A](l: scala.collection.immutable.List[A]): List[A] =
    l.foldLeft(nil[A])((a, b) => b :: a).reverse

  def nil[A]: List[A] = X.asInstanceOf[List[A]]

  def concat[A](xs: List[A], ys: List[A]): List[A] =
    xs match {
      case h Cons t => concat(t, h :: ys)
      case X        => ys
    }

  def reverse[A](list: List[A]): List[A] = {
    @tailrec def go(rest: List[A], result: List[A]): List[A] =
      rest match {
        case h Cons t => go(t, h :: result)
        case X        => result
      }
    go(list, nil)
  }
}

trait ListInstances {
  import control.Fold

  implicit val listFold: Fold[List] = new Fold[List] {
    override def fold[A, B](fa: List[A])(f: (A, B) => B, z: B): B = {
      def go(li: List[A], res: B): B = li match {
        case h Cons t => go(t, f(h, res))
        case X => res
      }
      go(fa, z)
    }
  }
}
