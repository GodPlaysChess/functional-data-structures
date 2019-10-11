package data.zipper

import control.Comonad
import data.tree.Tree


case class TreeZipper[A](
    parents: List[Tree[A]],
    left: List[Tree[A]],
    focus: A,
    right: List[Tree[A]],
    children: List[Tree[A]]
) {
  def moveParent: Option[TreeZipper[A]] = ???

  def moveChild: Option[TreeZipper[A]] = ???

  def moveLeft: Option[TreeZipper[A]] = ???

  def moveRight: Option[TreeZipper[A]] = ???

  def find(p: A => Boolean): Option[TreeZipper[A]] = ???

  def all(p: A => Boolean): Boolean = ???

  def modify(f: A => A): TreeZipper[A] = ???

  def modifyTree(f: Tree[A] => Tree[A]): TreeZipper[A] = ???

  def insertSiblingsLeft(siblings: Tree[A]): TreeZipper[A] = ???

  def insertSiblingsRight(siblings: Tree[A]): TreeZipper[A] = ???


}

object TreeZipper {
  implicit val Instance: Comonad[TreeZipper] = new Comonad[TreeZipper] {
    override def extract[A](fa: TreeZipper[A]) = ???

    override def duplicate[A](f: TreeZipper[A]) = ???

    override def map[A, B](fa: TreeZipper[A])(f: A => B) = ???
  }




}
