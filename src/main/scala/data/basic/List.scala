package data.basic

import control.Fold

import scala.annotation.tailrec
import scala.collection.immutable

sealed abstract class List[A] {
  def isEmpty: Boolean = cata(true, _ => false)

  def cata[B](caseNil: => B, caseCons: Cons[A] => B): B = this match {
    case c @ Cons(head, tail) => caseCons(c)
    case _: X.type            => caseNil
  }

  def ++(list: List[A]): List[A] = {
    def go(dec: List[A], res: List[A]): List[A] = dec match {
      case h Cons t => go(t, h :: res)
      case _        => res
    }
    go(reverse, list)
  }

  def ::(head: A): List[A] = Cons(head, this)

  def reverse: List[A] = List.reverse(this)

  def uncons: (Option[A], List[A]) = cata(None -> List.empty, c => Some(c.head) -> c.tail)

}

/**
 * trick with object X is to avoid invariance and use the same object for each empty List
 */
case class Cons[A](head: A, tail: List[A]) extends List[A]
case object X                              extends List[A forSome { type A }]

object List extends ListInstances {

  def apply[A](a: A*): List[A] =
    fromScala(a.toList)

  def asScala[A](l: List[A]): immutable.List[A] =
    Fold[List].fold(l)(immutable.List.empty[A])((a, b) => a :: b).reverse

  def fromScala[A](l: immutable.List[A]): List[A] =
    l.foldLeft(nil[A])((a, b) => b :: a).reverse

  def empty[A]: List[A] = nil[A]

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
    override def fold[A, B](fa: List[A])(z: B)(f: (A, B) => B): B = {
      def go(li: List[A], res: B): B = li match {
        case h Cons t => go(t, f(h, res))
        case X        => res
      }
      go(fa, z)
    }
  }
}
