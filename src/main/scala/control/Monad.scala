package control

trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](f: F[F[A]]): F[A]
}
