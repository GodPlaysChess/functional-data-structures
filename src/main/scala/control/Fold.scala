package control

trait Fold[F[_]] {
  // def foldMap[A, B](fa: F[A])(f: A => B, z: B): B

  def fold[A, B](fa: F[A])(z: B)(f: (A, B) => B): B
}

object Fold {
  def apply[F[_]: Fold]: Fold[F] = implicitly[Fold[F]]
}
