package control

trait Comonad[F[_]] extends Functor[F] {
  def extract[A](fa: F[A]): A

  def duplicate[A](f: F[A]): F[F[A]]
}
