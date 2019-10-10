package data.zipper

case class TreeZipper[A](
    parents: List[TreeZipper[A]],
    left: List[TreeZipper[A]],
    focus: A,
    right: List[TreeZipper[A]],
    children: List[TreeZipper[A]]
) {}
