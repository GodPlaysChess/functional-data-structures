package data.basic

import data.tree.Tree
import data.tree.Tree._
import control.Fold

class Set[A: Ordering](private val elements: Tree[A]) {
  def insert(a: A): Set[A] = new Set[A](elements.insert(a))

  def member(a: A): Boolean = elements.member(a)
}

object Set extends SetInstances {
  private final val Empty: Set[Unit] = new Set(Tree.empty[Unit])

  def empty[A: Ordering]: Set[A] = Empty.asInstanceOf[Set[A]]

  def one[A: Ordering](a: A): Set[A] = new Set(Tree.empty[A].insert(a))

  def fromFoldable[A: Ordering, F[_]: Fold](fa: F[A]): Set[A] =
    Fold[F].fold(fa)(Set.empty[A])((value, set) => set.insert(value))
}

trait SetInstances {}
