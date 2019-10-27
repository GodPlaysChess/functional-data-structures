package control

trait Fold[F[_]] {
  // def foldMap[A, B](fa: F[A])(f: A => B, z: B): B

  def fold[A, B](fa: F[A])(f: (A, B) => B, z: B): B
}
