package data.basic

import data.tree.Tree
import data.tree.Tree._
import control.Fold

class Set[A](private val elements: Tree[A]) {
  def insert(a: A): Set[A] = ???

  def member(a: A): Boolean = elements.member(a)
}

object Set extends SetInstances {
  private final val Empty: Set[Nothing] = ???

  def empty[A]: Set[A] = Empty.asInstanceOf[Set[A]]

  def create[A](a: A): Set[A] = ???

  def fromFoldable[A, F[_]: Fold](a: F[A]): Set[A] = ???
}

trait SetInstances {
 
}
